#' Exponential Decay
#'
#' @param A0 Initial value
#' @param k Decay slope (0: immediate decay; 1:no decay)
#' @param t Time step
#'
#' @return
#' @export
#'
#' @examples
decay_exp = function(A0, k,t) {
  return (A0*k^t)
}
#' Hyperbolic decay
#'
#' @param A0 Initial value
#' @param k Decay slope (0: no decay)
#' @param t Time step
#'
#' @return
#' @export
#'
#' @examples
decay_hyper = function(A0, k,t) {
  return (A0/(1+k*t))
}

#' Quasi-hyperbolic decay
#'
#' @param A0 Initial value
#' @param k_qexp Exponential decay component
#' @param k_qhyper Hyperbolic Decay component
#' @param t Time step
#'
#' @return
#' @export
#'
#' @examples
decay_qhyper = function(A0, k_qexp, k_qhyper, t) {
  if (t==0) {
    out=A0
  } else if (t>0) {
    out= k_qhyper*k_qexp^t*A0
  }
  return (out)
}

#' Hyperbolic growth
#'
#' @param A0 Initial value
#' @param k Exponential growth (0: no growth)
#' @param t Time step
#'
#' @return
#' @export
#'
#' @examples
growth_hyper <- function(A0, k, t) {
  return (A0/(1-k*t))
}
