#' Softmax function (2 values)
#'
#' @param vals vector of values (length==2)
#' @param temperature inverse temperature parameter (higher values -> more precision)
#'
#' @return
#' @export
#'
#' @examples
softmax <- function(vals, temperature) {
  p1 <- exp(vals[1]*temperature)
  p2 <- exp(vals[2]*temperature)
  prob <- p1/sum(p1,p2)
  return(prob)
}

#' Softmax function (2 values)
#'
#' @param vals vector of values (length==2)
#' @param temperature inverse temperature parameter (higher values -> more precision)
#'
#' @return
#' @export
#'
#' @examples
softmax2 <- function(vals, temperature) {
  #vals: vector [2] with estimates for the two options
  diff = vals[2]-vals[1]
  prob <- 1/(1+exp(diff/temperature))
  return(prob)
}


softmax3 <- function(par){
  n.par <- length(par)
  par1 <- sort(par, decreasing = TRUE)
  Lk <- par1[1]
  for (k in 1:(n.par-1)) {
    Lk <- max(par1[k+1], Lk) + log1p(exp(-abs(par1[k+1] - Lk)))
  }
  val <- exp(par - Lk)
  return(val)
}

#' Softmax (2 values, with inverted tau)
#'
#' @param vals vector of values (length==2)
#' @param tau temperature parameter (higher values -> less precision)
#'
#' @return
#' @export
#'
#' @examples
softmax_inverse <- function(vals,  tau) {
  out =(exp(vals[1]/tau))/((exp(vals[1]/tau))+(exp(vals[2]/tau)))#in softmax, probability of choosing alternative 1 depends on estimated probability of each alternative and tau (temperature) parameter
  return(out)
}


#' Thompson sampling (normal or beta (moment-matching method))
#'
#' @param par1 if pay_type == gauss: par1 <- vector of 2 means; if pay_type == beta: par1 <- vector of 2 alphas
#' @param par2  if pay_type == gauss: par1 <- vector of 2 sds;  if pay_type == beta: par2 <- vector of 2 betas
#' @param pay_type payoff type: drawn from either normal ('gauss') or beta ('beta') distribution
#'
#' @return
#' @export
#'
#' @examples
thompson= function (par1, par2, pay_type) {

  #normal approx: moment-matching
  if (pay_type=='beta') {
    mean_difference = beta_mean(alphas[1], betas[1]) - beta_mean(alphas[2], betas[2])
    sd_sum =          beta_sd  (alphas[1], betas[1]) + beta_sd  (alphas[2], betas[2])
  } else if (pay_type=='gauss') {
    mean_difference = par1[1] - par1[2]
    sd_sum          = par2[1] + par2[2]
  }

  return(pnorm(0, mean_difference, sd_sum, lower.tail = F))
}
