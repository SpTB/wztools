#' Calculate LOOIC of a cmdstan model object
#'
#' @param fit either draws or a character vector which can be retrieved using targets
#'
#' @return
#' @export
#'
#' @examples
calc_loo <- function(fit) {
  if (class(fit)[1] == 'character')       fit = targets::tar_read_raw(fit) #if input is a character, read it out
  if (class(fit)[1] == 'CmdStanMCMC_CSV') { # reading chain-based csv files case
    draws = fit$draws()
    fit = apply(draws, c(1,3), mean) %>% as_tibble() #average across chains
  }
  loglik = fit %>%
    select(contains('log_lik')) %>%
    select(where(~!any(is.na(.))))
  out = loo::loo.array(posterior::as_draws_array(loglik))
  return(out)
}
#' Calculate WAIC (and llpd) of a cmdstan model object
#'
#' @param fit either draws or a character vector which can be retrieved using targets
#'
#' @return
#' @export
#'
#' @examples
calc_waic<-function(fit) {
  if (class(fit)[1] == 'character') fit = targets::tar_read_raw(fit) #if input is a character, read it out
  if (class(fit)[1] == 'CmdStanMCMC_CSV') { # reading chain-based csv files case
    draws = fit$draws()
    fit = apply(draws, c(1,3), mean) %>% as_tibble() #average across chains
  }
  loglik = fit %>%
    select(contains('log_lik')) %>%
    select(where(~!any(is.na(.))))
  out = LaplacesDemon::WAIC(t(loglik))
  return(out)
}
