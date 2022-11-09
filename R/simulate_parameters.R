#' Simulate group means (mus)  from beta-like priors
#'
#' @param mu_list
#' @param sigma_list
#' @param range_list
#' @param sigma
#' @param range
#' @param nsim
#' @param seed
#'
#' @return
#' @export
#'
#' @examples
sim_mus <-function(mu_list, sigma_list=NULL, range_list=NULL, sigma=0.1, range=c(0,1), nsim, seed=NULL) {

  if (!is.null(seed)) set.seed(seed)
  out=list()
  for (i in seq_along(1:length(mu_list))) {
    namei = paste0('mu_',substr(names(mu_list[i]), 4, nchar(names(mu_list[i]))))
    if (!is.null(sigma_list)) sigmai = sigma_list[[i]] else sigmai = sigma
    if (!is.null(range_list)) rangei = range_list[[i]] else rangei = range
    out[[namei]] = wztools::draw_from_beta_hypers(native_mu=mu_list[[i]], sigma=sigmai, native_range=rangei, nsub=nsim)
  }

  return (out)
}


#' Simulate individual parameters from beta-like group-level priors (with lower and upper bounds)
#'
#' @description The function takes in group-level means ('mu' parameters), standard deviations ('sigmas')
#' @param mu_list A \code{list} of group-level means. Each element needs to be named as the individual parameter.
#' @param sigma_list (optional) A \code{list} of group-level standard deviation parameters, in beta distribution-space. If all sigmas are the same, this argument can stay as \code{NULL}
#' @param range_list (optional) A \code{list} of group-level lower and upper bounds. If all bounds are the same, this argument can be skipped
#' @param sigma (optional) Single value specifying all group-level standard deviation parameters
#' @param range (optional) Single \code{list} specifying all group-level parameter ranges (\code{c(lb, ub)} format).
#' @param nsub number of participants
#' @param seed self-explanatory

#' @return A \code{list} containing individual parameters
#' @export
#'
#' @examples
sim_ind_pars <-function(mu_list, sigma_list=NULL, range_list=NULL, sigma=0.1, range=c(0,1), nsub, seed=NULL) {

  if (!is.null(seed)) set.seed(seed)
  out=list()
  for (i in seq_along(1:length(mu_list))) {
    namei = substr(names(mu_list[i]), 4, nchar(names(mu_list[i])))
    if (!is.null(sigma_list)) sigmai = sigma_list[[i]] else sigmai = sigma
    if (!is.null(range_list)) rangei = range_list[[i]] else rangei = range
    out[[namei]] = wztools::draw_from_beta_hypers(native_mu=mu_list[[i]], sigma=sigmai, native_range=rangei, nsub=nsub)
  }

  return (out)
}


#' Simulate individual parameters from beta-like group-level priors (with lower and upper bounds) for multiple simulations with different parameter settings
#'
#' @param exp_struct_list list specifying the experimental structure (current version needs only the nsub field, specifying the number of participants)
#' @param mu_list hierarchical list specifying group parameter means (e.g.\code{list(mu_alpha=c(0.25,0.5,0.75),mu_tau  =c(0.25,0.5,0.75))}
#' @param seed you know what a seed is
#'
#' @return
#' @export
#'
#' @examples
sim_ind_pars_multi = function(exp_struct_list, mu_list, sigma_list=NULL, range_list=NULL, sigma=0.1, range=c(0,1), seed=NULL) {
  if(!is.null(seed)) set.seed(seed)
  list2env(exp_struct_list, envir = environment())
  par_combos = expand.grid(mu_list)

  ind_pars=list()
  for (i in seq_along(1:nrow(par_combos))) { #simulation loop
    mu_listi = list()
    for (nam in names(par_combos)) { #parameter loop
      mu_listi[[nam]] = par_combos[[nam]][i]
    }
    ind_pars[[i]] = sim_ind_pars(mu_list=mu_listi, nsub=nsub, sigma_list=sigma_list, range_list=range_list)
  }
  return(ind_pars)
}


