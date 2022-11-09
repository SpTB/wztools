##ALL FUNCTIONS THAT INTERACT WITH STAN OR STAN-DERIVED OBJECTS

compile_model <- function(model_file) {
  cmdstan_model(model_file)
  return (model_file)
}

#' Making a stan-compatible data list
#'
#' @param dat original dataframe
#' @param var_names output variables (choice, rt, outcome, etc...)
#' @param grouping_vars grouping variables (subjID, games, trials)
#' @param grouping_vars_stan_names specifying the grouping variable stan-names
#' @param hierarchical whether the dataset is multi-subject (T) or not (F)
#' @param ssm_prepro whether to include lower RT bounds for sequential sampling models input
#'
#' @return
#' @import dplyr
#' @export
#'
#' @examples
make_stan_list <- function(dat, var_names, grouping_vars = c('subjID', 'game', 'trial'), grouping_vars_stan_names=c('numSubj', 'numGames', 'numTrials'), hierarchical = T, ssm_prepro=F) {
  # transforms a data df into a list for stan

  #remove unnecessary columns, like parameter values, etc:
  dat = dat %>%
    select_if(names(.) %in% c(var_names, grouping_vars))

  #case: e1 c('choice', 'choseA', 'outcome', 'free', 'cond','mixed','game','trial','subjID','rated','rating', 'rt'))

  #divide variables by their role
  names(grouping_vars) = grouping_vars_stan_names

  #if (!hierarchical) { #remove subject grouping if only one
  #  grouping_vars = grouping_vars[2:length(grouping_vars)]
  #}

  looping_vars = names(dat)[!(names(dat) %in% grouping_vars)] #output vars


  #get meta pars (how many participants, games, choices per game)
  stan_data<-list()
  for(col in grouping_vars) {
    out_name = names(grouping_vars)[grouping_vars == col]
    stan_data[[out_name]] = max(dat[[col]])
  }

  #if rt in data, preprocess rt vars necessary for ddm
  if ('rt' %in% names(dat) & ssm_prepro) {
    stan_data$RTbound = min(dat$rt[dat$rt>-1])
    if (hierarchical) {
      stan_data$minRT = dat %>% filter(rt>-1) %>% group_by(subjID) %>% summarise(min(rt))  %>% tibble::deframe()
    }
  }

  for (col in looping_vars) {
    stan_data[[col]] =  dat %>%
      group_by(across(all_of(unname(grouping_vars)))) %>%  #subjID, game, trial
      select(col) %>%
      tidyr::pivot_wider(names_from = trial, values_from=col) %>%
      arrange(subjID)
    #ungroup()
  }


  #hierarchical
  if (hierarchical) {

    #this part was to make an rt+choice double array for LBA, but not useful anymore
    # if (join_rt_choice) {
    #   choice = split (stan_data$choice, stan_data$choice$subjID) %>% #split by subj (separate lists)
    #     lapply( FUN = function(x) x[,3:ncol(x)]) %>%  #remove grouping
    #     lapply(as_vector) %>% #reduce dims
    #     abind::abind(along=2) # list to array
    #   choice = choice[choice>0] #remove missing data (due to RTs)
    #   rt = split (stan_data$rt, stan_data$rt$subjID) %>% #split by subj (separate lists)
    #     lapply( FUN = function(x) x[,3:ncol(x)]) %>%  #remove grouping
    #     lapply(as_vector) %>% #reduce dims
    #     abind::abind(along=2) # list to array
    #   rt = rt[rt>0] #remove missing data (due to RTs)
    #   stan_data$RT = abind::abind(rt, choice, along=3) #join
    # }

    for (nam in looping_vars) {
      stan_data[[nam]] = split (stan_data[[nam]], stan_data[[nam]]$subjID)
      stan_data[[nam]] = lapply(stan_data[[nam]], FUN = function(x) x[,3:ncol(x)]) # remove grouping cols
      stan_data[[nam]] = abind::abind(stan_data[[nam]], along=3) # create array (dims: games, trials, subjs)
      stan_data[[nam]] = aperm(stan_data[[nam]], c(3,1,2)) # rearrange dims: subjID, games, trials
    }

  } else { #single subj: remove grouping game column
    for (nam in looping_vars) {
      stan_data[[nam]] =  stan_data[[nam]][,2:ncol(stan_data[[nam]])] %>% as.matrix()
    }
    #stan_data$outcome  = stan_data$outcome[,2:ncol(stan_data$outcome)] %>% as.matrix()
  }
  return (stan_data)
}

#' @title Fit a stan model with cmdstanr default fitting setup
#' @description A wrapper around \code{cmdstanr::cmdstan_model}
#' @param stan_data A \code{list} with data, formatted for the stan model
#' @param model_file (char) model path
#' @param summary_only (bool)
#' @param iter_warmup (int) warmup interations
#' @param iter_sampling (int) sampling iterations
#' @param chains (int) number of chains
#' @param parallel_chains (int) how many chains in parallel
#' @param refresh (int) how often to print info
#' @param seed (int) seed
#'
#' @return
#' @export
#'
#' @examples
fit_model <- function(stan_data, model_file, summary_only=T, iter_warmup=2000,iter_sampling=2000, chains=2, parallel_chains=2, refresh=500, seed=1, ...) {

  # truth <- data$beta_true[1]
  model <- cmdstanr::cmdstan_model(model_file)
  fit <- model$sample(
    data = stan_data,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    seed = seed,
    chains = chains,
    parallel_chains = parallel_chains,
    refresh = refresh,
    ...
  )
  if (summary_only)  fit = fit$summary() #   filter(variable == "beta") %>%
  #   mutate(beta_true = truth, cover_beta = q5 < truth & truth < q95)
  return (fit)
}


#' Adds columns specifying true mu, 95% coverage, bias and precision to the cmdstanr fit summary dataframe containing only mu rows
#'
#' @param mu_fit filtered cmdstanr summary fit dataframe (only mu rows)
#' @param mu_list named list containing true mu values
#' @param model_num (int) specifying the number of stan models used in the dataframe
#'
#' @return
#' @export
#'
#' @examples
add_cov_stats_mu = function(mu_fit, mu_list, model_num=1) {
  par_combos = expand.grid(mu_list)
  true_mu_df = par_combos |> slice(rep(1:n(), each=model_num))

  #make sure par names in the model and col names in true_mu_df are in the same order!
  mu_names = unique(mu_fit$variable)
  true_mu_df = true_mu_df[mu_names]

  mu_fit$true_mu = as.vector(t(true_mu_df))
  mu_fit = mu_fit |> mutate(
    cov95 = (q5 <= true_mu) & (q95>= true_mu),
    cov95_wid = q95-q5,
    bias = mean - true_mu,
    prec = abs(mean - true_mu)
  )

  return (mu_fit)
}


#' Filters cmdstan fit summary dataframe for mu values and adds fit diagnostic columns (add_cov_stats_mu())
#'
#' @param stan_fit_summary  cmdstan fit summary dataframe
#' @param mu_list named list containing true mu values
#' @param model_num (int) specifying the number of stan models used in the dataframe
#'
#' @return
#' @export
#'
#' @examples
get_mus <- function(stan_fit_summary, mu_list, model_num=1) {
  mus = stan_fit_summary |>
    filter(str_detect(variable, 'mu') & !str_detect(variable, 'raw'))
  out = add_cov_stats_mu(mu_fit = mus, mu_list=mu_list, model_num=model_num)
  return (out)
}

#' Adds columns specifying true mu, 95% coverage, bias and precision to the cmdstanr fit summary dataframe containing only individual parameter rows
#'
#' @param ind_fit filtered cmdstanr summary fit dataframe (only individual par rows)
#' @param ind_pars list containing true individual parameters
#' @param model_num (int) specifying the number of stan models used in the dataframe
#'
#' @return
#' @export
#'
#' @examples
add_cov_stats_ind <- function(ind_fit, ind_pars, model_num) {
  nsub = length(ind_pars[[1]][[1]])
  npar = length(ind_pars[[1]])
  ind_fit$true = rep(unlist(ind_pars),model_num)
  ind_fit$sim_num = rep(rep(1:length(ind_pars), each = nsub*npar), model_num)
  ind_fit = ind_fit |> mutate(
    cov95 = (q5 <= true) & (q95>= true),
    cov95_wid = q95-q5,
    bias = mean - true,
    prec = abs(mean - true)
  )
}

#wrapper around
#' Filters cmdstan fit summary dataframe for mu values and adds fit diagnostic columns (add_cov_stats_ind())
#'
#' @param stan_fit_summary  cmdstan fit summary dataframe
#' @param ind_pars list containing true individual parameters
#' @param model_num (int) specifying the number of stan models used in the dataframe
#'
#' @return
#' @export
#'
#' @examples
get_inds <- function(stan_fit_summary, ind_pars, model_num, param_list=NULL) {
  search_vect = NULL
  ind_par_names = names(ind_pars[[1]])
  #extract a list of individual parameters followed by '[x]'

  if (is.null(param_list)) {
    for (i in seq_along(1:length(ind_par_names))) {
      curr_nam = paste0(ind_par_names[i], '\\[')
      search_vect = c(search_vect, curr_nam)
    }
  } else {
    for(i in seq_along(1:length(param_list))) {
      search_vect = c(search_vect, paste0(param_list[i], "\\["))
    }
  }
  inds = stan_fit_summary |>
    filter(str_detect(variable, paste(search_vect,collapse = '|'))) |>
    arrange(model_type) |>
    mutate(var_type = sub("\\[\\b.*",'',variable),
           subjID = as.numeric(str_extract(variable, "\\d+"))) #|>
  #select(-variable)

  #add simulation number
  sim_num = 1:(nrow(inds)/(max(inds$subjID)*length(unique(inds$var_type))))
  inds$sim_num=rep(sim_num ,each=nrow(inds)/length(sim_num))

  # arrange based on ind_pars
  inds = inds |>
    arrange(sim_num,factor(var_type, levels = ind_par_names))
  out = add_cov_stats_ind(ind_fit=inds, ind_pars=ind_pars, model_num=model_num)
  return(out)
}


#' Calculate LOOIC of a cmdstan model object
#'
#' @param fit either draws or a character vector which can be retrieved using targets
#'
#' @return
#' @export
#'
#' @examples
calc_loo <- function(fit) {
  if (class(fit)[1] == 'character') fit = targets::tar_read_raw(fit) #if input is a character, read it out
  loglik = fit %>%
    select(starts_with('log_lik')) %>%
    select(where(~!any(is.na(.))))
  out = loo::loo.array(posterior::as_draws_array(loglik))
  return(out)
}
