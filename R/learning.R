#' Kalman filter
#'
#' @param vals vector of 2 values
#' @param vars vector od 2 standard deviations
#' @param num_chosen int specifying which option was chosen (1 or 2)
#' @param reward reward value
#' @param sigma_noise estimation noise parameter
#' @param sigma_change environmental stability parameter
#'
#' @return
#' @export
#'
#' @examples
kalman <- function(vals, vars, num_chosen, reward, sigma_noise, sigma_change) {
  # vars: variances vector
  # num_chosen: number of chosen bandit (1 or 2)

  vars = vars^2 # square stds to get variances
  # set the Kalman gain for unchosen options
  kt <- rep(0,2)
  # set the Kalman gain for the chosen option
  kt[num_chosen] <- (vars[num_chosen] +
                       sigma_change^2)/(vars[num_chosen] +
                                        sigma_change^2 + sigma_noise^2)
  #print (kt)
  # compute the posterior means
  vals_out <- vals + kt*(reward - vals)
  # compute posterior variances
  vars_out <- (1-kt)*(vars + sigma_change^2)

  vars_out = sqrt(vars_out) # back to std
  out = list(vals_out, vars_out)
  return(out)
}

