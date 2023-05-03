#' Extract group-level mean parameters (mus) from cmdstanr model summary object
#'
#' @param stan_fit_summary
#'
#' @return
#' @export
#'
#' @examples
get_mus2 <- function(stan_fit_summary) {
  mus = stan_fit_summary |>
    filter(str_detect(variable, 'mu') & !str_detect(variable, 'raw'))
  return(mus)
}

#' Extract individual parameters from cmdstanr model summary object
#'
#' @param stan_fit_summary
#' @param ind_pars
#'
#' @return
#' @export
#'
#' @examples
get_inds2<-function(stan_fit_summary, ind_pars=NULL) {
  #extract a list of individual parameters followed by '[x]'
  if (is.null(ind_pars)) {
    inds= stan_fit_summary %>% filter(str_detect(variable, '\\[') & !str_detect(variable, 'raw') & !str_detect(variable, 'log_lik') & !str_detect(variable, 'y_pred'))
  } else {
    search_vect = map_chr(ind_pars, ~paste0(.x, '\\['))
    inds = stan_fit_summary %>%
      filter(str_starts(variable,    paste(search_vect,collapse = '|'))) %>%
     # filter(!str_detect(variable, 'log_lik')) %>%
    #  filter(!str_detect(variable, 'y_pred')) %>%
      mutate(var_type = sub("\\[\\b.*",'',variable),
             subjID = as.numeric(str_extract(variable, "\\d+")))
  }

  return(inds)
}

## FOR WORKING WITH PRIOR SIMS

#' Extracts simulated pars
#'
#' @param cmdstan_summary
#' @param preds a list of predictor names
#'
#' @return
#' @export
#'
#' @examples
extract_preds <-function(cmdstan_summary, preds = c('d_pred', 'r_pred')) {
  out = cmdstan_summary %>%
    filter(str_detect(variable,paste(preds, collapse='|'))) %>% mutate(
      trial = str_extract(variable, '\\b[^,]+$') %>% parse_number(),
      game = str_extract(variable, '(?<=,)[^,]+(?=,)') %>% parse_number(),
      subjID =gsub("^(.*?),.*", "\\1", variable) %>% parse_number()
    ) %>% arrange(subjID, game, trial) %>%
    mutate(rounded_pred = round(mean)) %>%
    select(variable, mean, subjID, game, trial)
  return (out)
}


#' Turns multiple variables from a cmdstan_summary df to separate columns using pivot_wider
#'
#' @param cmdstan_summary
#'
#' @return
#' @export
#'
#' @examples
widen_preds <- function(cmdstan_summary) {
  out = cmdstan_summary %>%
    mutate(var_type = sub("\\[\\b.*",'',variable)) %>%
    select(-variable) %>%
    pivot_wider(names_from = var_type, values_from = mean)
}

#' Extracts individual parameter values from the task skeleton back to a list
#'
#' @param dataframe task skeleton
#' @param mu_list
#' @param grouping_cols for individual parameters only subjID
#'
#' @return
#' @export
#'
#' @examples
pars_to_stan <- function(dataframe, mu_list ,grouping_cols = c('subjID')) {
  par_names = paste0(substr(names(mu_list), 4, nchar(names(mu_list))), '_true')

  #remove unncecessary cols
  dataframe = dataframe %>%
    select_if(names(.) %in% c(par_names, grouping_cols))

  dataframe = dataframe %>% group_by(subjID) %>% summarise_all(max) %>% select_if(!names(.) %in% grouping_cols)
  #remove the 'true' part of variable name
  names(dataframe) = substr(names(dataframe), 1, nchar(names(dataframe))-5)

  return(as.list(dataframe))
}
