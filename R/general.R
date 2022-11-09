
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

#' Sources all the code within given folder + file name pattern
#'
#' @param path character vector specifying the path to the folder with to-be-sourced code
#' @param pattern character vector specifying the file name pattern
#'
#' @return 0
#' @export
#'
#' @examples
sourceAll <- function(path, pattern='_func') {
  fi = list.files(path =path,
                  pattern=pattern,
                  full.names = T )

  for (i in fi) {
    source(i)
  }
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
