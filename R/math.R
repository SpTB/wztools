softplus <-function(x) {
  return(log(1+exp(x)))
}

#' Sigmoidal drift-rate transform (based on: https://pubmed.ncbi.nlm.nih.gov/30924057/)
#'
#' @param z original drift rate
#' @param vmax maximum drift rate (abs)
#'
#' @return
#' @export
#'
#' @examples
sigm_drift<-function(z, vmax) {
  return ((2*vmax)/(1+exp(-z))-vmax)
}
