#' @title 10-subject data simulation
#'
#' @description Simulated data \code{tibble} containing responses of 10 synthetic subjects (accuracies and rts) in two difficulty conditions
#'
#' @format A \code{tibble} with 4 columns, which are:
#' \describe{
#' \item{subjID}{participant number}
#' \item{cond}{Difficulty condition ('Easy' or 'Difficult')}
#' \item{trial}{trial number per difficulty condition}
#' \item{choice}{Accuracy (binary)}
#' \item{rt}{Reaction Time (s), bound between 0.2 and 3 (simulated from a truncated normal distribution)}
#' }
"sim"
