
#' Setting various options
#'
#' @param ... parameters passed to specific setXOptions funcs. Currently only for GGoptions.
#'
#' @return NA
#' @export
#'
#' @examples
setOptions  <- function(...) {
  setGGoptions(...)
  options(dplyr.summarise.inform = FALSE)
  options(scipen=999)
}

#' Variable name to string conversion
#'
#' @param variable Variable name
#'
#' @return
#' @export
#'
#' @examples
varToStr <- function(variable) {
  return (deparse(substitute(variable)))
}
