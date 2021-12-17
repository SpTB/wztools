#' Rdata Loader
#'
#'Loads loads an RData file and returns it
#' @param fileName Rdata object
#'
#' @return
#' @export
#'
loadRData <- function(fileName){
  #
  load(fileName)
  return (get(ls()[ls() != "fileName"]))
}
