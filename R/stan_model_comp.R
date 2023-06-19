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





#' Extract LOOIC values from all models in your targets folder ('draws' objects)
#'
#' @param model_dir Specifies the targets directory (default should work)
#' @param model_str Specifies model string
#'
#' @return a list of loo scores
#' @export
#'
#' @examples
extract_loos <- function(model_dir ='_targets/objects/', model_str=NULL) {
  files = list.files(model_dir, pattern = paste0(model_str,'_draws'))
  loo_scores = map(files, wztools::calc_loo)
  return (loo_scores)
}

#' Extract WAIC values from all models in your targets folder ('draws' objects)
#'
#' @param model_dir Specifies the targets directory (default should work)
#' @param model_str Specifies model string
#'
#' @return a list of WAIC scores
#' @export
#'
#' @examples
extract_waics <- function(model_dir ='_targets/objects/', model_str=NULL) {
  files = list.files(model_dir, pattern = paste0(model_str,'_draws'))
  waic_scores = map(files, wztools::calc_waic)
  return (waic_scores)
}


#' Creates a table of loo/waic scores based on loo lists from `extract_loos` and `extract_waics` functions
#'
#' @param loo_list a list of model loo values created with `extract_loos` function
#' @param waic_list a list of model WAIC values created with `extract_waics` function
#' @param model_str Specifies model string
#' @param relevant_pars character string (seems superfluous, since the function should automatically detect which parameters to extract based on the first 2 arguments)
#'
#' @return
#' @export
#'
#' @examples
model_comp_table <- function(loo_list=NULL, waic_list=NULL, model_str=NULL, relevant_pars = 'looic') {

  if(is.null(loo_list) & is.null(waic_list)) stop('Either LOO or a WAIC list needs to be provided')
  if(is.null(model_str)) stop('Please provide the model string argument (name of the stan_mcmc target)')
  files = list.files('_targets/objects/', pattern = paste0(model_str,'_draws'))

  loo_table = waic_table=NULL
  if (!is.null(loo_list)) {
    #necessary for alternative method, when extracting looic no longer possible (deprecated)
    estimate_list = map(loo_list, pluck, 'estimates')
    estimate_mat = do.call('rbind', estimate_list)
    looic_vect = subset(estimate_mat, rownames(estimate_mat) %in% relevant_pars)[,1] |>as.vector()

    loo_table = tibble(
      model = files,
      #loo = looic_vect #alternative method, when the original stops working
      loo = map(loo_list, pluck, 'looic') %>% unlist()
    )
  }

  if (!is.null(waic_list)) {
    waic_table = tibble(
      model = files,
      waic = map_dbl(waic_list, pluck, 'WAIC')
    )
  }

  if (!is.null(loo_table) & !is.null(waic_table)) {
    out_table = full_join(loo_table, waic_table) %>% arrange(loo)
  } else if (!is.null(loo_table) & is.null(waic_table)) {
    out_table = loo_table %>% arrange(loo)
  } else {
    out_table = waic_table %>% arrange(waic)
  }

  return(out_table)
}
