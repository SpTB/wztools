#' @title Fit a stan model with cmdstanr default fitting setup
#' @description A wrapper around \code{cmdstanr::cmdstan_model}
#' @param stan_data A \code{list} with data, formatted for the stan model
#' @param model_file (char) model path
#' @param summary_only (bool)
#' @param iter_warmup (int) warmup interations
#' @param iter_sampling (int) sampling iterations
#' @param chains (int) number of chains
#' @param parallel_chains (int) how many chains in parallel
#' @param refresh (int) how often to print info
#' @param seed (int) seed
#'
#' @return
#' @export
#'
#' @examples
fit_model <- function(stan_data, model_file, summary_only=T, iter_warmup=2000,iter_sampling=2000, chains=2, parallel_chains=2, refresh=500, seed=1, ...) {

  # truth <- data$beta_true[1]
  model <- cmdstanr::cmdstan_model(model_file)
  fit <- model$sample(
    data = stan_data,
    iter_warmup = iter_warmup,
    iter_sampling = iter_sampling,
    seed = seed,
    chains = chains,
    parallel_chains = parallel_chains,
    refresh = refresh,
    ...
  )
  if (summary_only)  fit = fit$summary() #   filter(variable == "beta") %>%
  #   mutate(beta_true = truth, cover_beta = q5 < truth & truth < q95)
  return (fit)
}

#' Simulate a synthetic dataset using a stan model and a parameter list
#'
#' @param stan_data stan list containing task skeleton + individual parameter values
#' @param model_file .stan file with generated quantities block
#' @param summary_only
#'
#' @return
#' @export
#'
#' @examples
fit_model_gen <- function(stan_data, model_file, summary_only=T) {

  # truth <- data$beta_true[1]
  model <- cmdstan_model(model_file)
  fit <- model$sample(
    data = stan_data,
    iter_warmup = 0,
    iter_sampling = 1,
    fixed_param=T,
    seed = 1,
    chains = 1
  )
  if (summary_only)  fit = fit$summary() #   filter(variable == "beta") %>%
  #   mutate(beta_true = truth, cover_beta = q5 < truth & truth < q95)
  return (fit)
}

