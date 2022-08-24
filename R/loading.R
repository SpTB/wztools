#' Rdata Loader
#'
#'Loads loads an RData file and returns it (can assign it to a new variable)
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
