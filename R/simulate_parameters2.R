#' Title
#'
#' @desription Draw a parameter mean from a re-scaled beta distribution defined by alpha and beta drawn from gamma distributions
#'
#' @param gamma_parsA gamma priors for alpha parameter of the Beta distribution
#' @param gamma_parsB gamma priors for beta parameter of the Beta distribution
#' @param shift
#' @param scale
#' @param nsim
#'
#' @return
#' @export
#'
#' @examples
draw_mu_from_gamma <-function(gamma_parsA=c(1,1), gamma_parsB=c(1,1), shift=0, scale=1, nsim) {
  alpha = rgamma(n=nsim, gamma_parsA[1], gamma_parsA[2])+1
  beta = rgamma(n=nsim, gamma_parsB[1], gamma_parsB[2])+1
  mu_beta = alpha/(alpha+beta) * scale + shift
  return (mu_beta)
} # ++

#' Title
#'
#' @param model_struct_list Model struture list
#' @param prior_dist prior distribution (currently works for 'gamma' (default), 'beta' and 'gauss')
#' @param nsim number of simulations
#' @param seed
#'
#' @return
#' @export
#'
#' @examples
sim_mus2 <-function(model_struct_list, prior_dist='gamma', nsim, seed=NULL) {

  if (!is.null(seed)) set.seed(seed)
  out=list()

  prior_comb = model_struct_list
  for (i in seq_along(1:length(prior_comb))) {
    namei = paste0('mu_', names(prior_comb[i]))
    #out[[names(prior_comb[i])]] = NULL
    if ('fixed_value' %in% names(prior_comb[[i]])) {
      out[[namei]]$value = prior_comb[[i]]$fixed_value
      out[[namei]]$fixed = T
    }
    else {
      if (prior_dist=='beta') {
        out[[namei]] = wztools::draw_from_beta_hypers(native_mu   =prior_comb[[i]]$mu,
                                                      sigma       =prior_comb[[i]]$sigma,
                                                      native_range=prior_comb[[i]]$range,
                                                      nsub=nsim)
        out[[namei]]$fixed = F
      } else if (prior_dist=='gamma') {

        out[[namei]]$value = draw_mu_from_gamma(gamma_parsA=prior_comb[[i]]$gamma_parsA,
                                                gamma_parsB=prior_comb[[i]]$gamma_parsB,
                                                shift =prior_comb[[i]]$range[1],
                                                scale = prior_comb[[i]]$range[2]-prior_comb[[i]]$range[1],
                                                nsim=nsim)
        out[[namei]]$fixed = F


      } else if (prior_dist=='gauss') {
        out[[namei]] = rnorm(mean =prior_comb[[i]]$mu,
                             sd   =prior_comb[[i]]$sd,
                             n=nsim)
        out[[namei]]$fixed = F
      }
    } # if/else fixed val
  } #parameter loop
  return (out)
}


