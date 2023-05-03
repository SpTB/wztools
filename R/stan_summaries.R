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
