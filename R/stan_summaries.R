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
get_inds <- function(stan_fit_summary, ind_pars, model_num, param_list=NULL, many_models=F) {
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
    #arrange(model_type) |>
    mutate(var_type = sub("\\[\\b.*",'',variable),
           subjID = as.numeric(str_extract(variable, "\\d+")))

  #arrange by model type if many models
  if (many_models) inds = inds |> arrange(model_type)

  #add simulation number
  sim_num = 1:(nrow(inds)/(max(inds$subjID)*length(unique(inds$var_type))))
  inds$sim_num=rep(sim_num ,each=nrow(inds)/length(sim_num))

  # arrange based on ind_pars
  inds = inds |>
    arrange(sim_num,factor(var_type, levels = ind_par_names))
  out = add_cov_stats_ind(ind_fit=inds, ind_pars=ind_pars, model_num=model_num)
  return(out)
}



#' Adds variables from stan_list to to stan model summary (3-D, 2-D or 1-D)
#' @description Useful for supplying simulated model with simulated parameters and experimental vars from task skeleton
#' @param exp_struct_list has to have 'ntrial' and 'nblock' field, specifying the number of trials per subject
#' @param cmdstan_summary summary object
#' @param stan_list stan list containing the variables to add
#' @param var_names names of variables to add
#'
#' @return
#' @export
#'
#' @examples
add_vars_from_stan_list_to_model_output <- function(exp_struct_list, cmdstan_summary, stan_list, var_names) {
  for (nam in var_names) {

    if (length(dim(stan_list[[nam]]))==3) { #3-D var case
      cmdstan_summary[[nam]] = as.vector(aperm(stan_list[[nam]] ,c(3,2,1)))
    } else if (length(dim(stan_list[[nam]]))==2) { #2-D var case
      cmdstan_summary[[nam]] = rep(as.vector(aperm(stan_list[[nam]] ,c(2,1))), each=exp_struct_list$ntrial)
    } else if (length(dim(stan_list[[nam]]))==0) { #1-D var case
      cmdstan_summary[[nam]] = rep(stan_list[[nam]] ,each=exp_struct_list$ntrial*exp_struct_list$nblock)
    }
  }
  return (cmdstan_summary)
}



#' Adds individual parameters from task skeleton to a stan list
#'
#' @param dataframe task skeleton df
#' @param mu_list
#' @param grouping_cols
#'
#' @return
#' @export
#'
#' @examples
pars_to_stan <- function(dataframe, mu_list ,grouping_cols = c('subjID')) {
  par_names = paste0(substr(names(mu_list), 4, nchar(names(mu_list))), '_true')

  #remove unncecessary cols
  dataframe = dataframe %>%
    select_if(names(.) %in% c(par_names, grouping_cols))

  dataframe = dataframe %>% group_by(subjID) %>% summarise_all(max) %>% select_if(!names(.) %in% grouping_cols)
  #remove the 'true' part of variable name
  names(dataframe) = gsub('_true', '', names(dataframe))#        substr(names(dataframe), 1, nchar(names(dataframe))-5)

  return(as.list(dataframe))
}
