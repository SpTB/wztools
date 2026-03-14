#' Estimate alpha and beta parameters of the Beta distribution based on the mean and std or variance
#'
#' @param mu mean
#' @param var variance
#' @param std standard deviation
#'
#' @return list of parameters
#' @export
#'
#' @examples
estBetaParams <- function(mu, var=NULL, std=NULL) {
  if(is.null(std) && is.null(var)) stop ('Please provide either the desired stardand deviation (std) or variance (var) argument')
  if(!is.null(std) && !is.null(var)) stop ('Please provide only 1 dispertion parameter: either stardand deviation (std) or variance (var) ')
  if(!is.null(std)) var=std^2
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}

#' Estimate mean of the beta distribution
#'
#' @param alpha
#' @param beta
#'
#' @return Mean of the Beta distribution
#' @export
#'
#' @examples
beta_mean <- function(alpha, beta) {
  return(alpha/(alpha+beta))
}

#' Estimate the standard deviation of the Beta distribution
#'
#' @param alpha Alpha parameter of the Beta distribution
#' @param beta Beta parameter of the Beta distribution
#'
#' @return
#' @export
#'
#' @examples
beta_sd <-function(alpha, beta) {
  out = sqrt( (alpha*beta) / ((alpha+beta)^2 * (alpha+beta+1)))
  return (out)
}


#' Draw individual parameters from beta-transformed hyper-parameter
#'

#' @param native_mu Mean of the hyper-parameter in native scale
#' @param sigma Std of the hyper-parameter in beta-scale
#' @param native_range range of the native scale (e.g. c(-5,5))
#' @param nsub number of subjects
#'
#' @return Individual parameter vector (in native scale)
#' @export
#'
#' @examples
draw_from_beta_hypers <- function (native_mu, sigma, native_range, nsub) {
  scaled_mu = scales::rescale(native_mu, from=native_range, to=c(0,1))
  scaled_ab = estBetaParams(mu = scaled_mu, std=sigma)
  native = scales::rescale(stats::rbeta(nsub, scaled_ab$alpha, scaled_ab$beta), from=c(0,1), to=native_range)
  return (native)
}


#' transforms raw (in this case beta parameters) to their native space
#'  by calculating the mean, and scaling and shifting.
#'
#' @param raw_df df contining columns with alpha and beta parameters
#' @param scale_vector 1 scale value for each native parameter
#' @param shift_vector
#' @param prefix string to add at the start (e.g. from 'alpha' to 'mu_alpha')
#'
#' @return
#' @export
#'
#' @examples
raw_to_native = function(raw_df, scale_vector, shift_vector ,prefix='') {
  prefix_alpha = paste0(prefix ,'a_')
  prefix_beta = paste0(prefix ,'b_')
  alphas = raw_df %>% select(starts_with(prefix_alpha))
  betas = raw_df %>% select(starts_with(prefix_beta))
  means = alphas/(alphas+betas)
  names(means) = stringr::str_remove(names(means), 'a_')
  out = map2_dfr(means, scale_vector, `*`)
  out = map2_dfr(out, shift_vector, `+`)
}
