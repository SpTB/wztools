#' Extract simulated variables from the generated quantities block
#' @description The summary object is assumed to to be 2D (subjID, trial) or 3D (subjID, game, trial), but should work for 1D -> in 1D case (leaves only trials as the grouping var)
#' @param cmdstan_summary a cmdstan summary object
#' @param preds character vector specifying names of the predictors
#'
#' @return a dataframe containing predictions
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

  #remove NA-only cols (2-D case)
  out  = out %>% select_if(~ !all(is.na(.)))

  #remove subjID (1-D data case)
  if (mean(out$subjID==out$trial)) out$subjID = NULL

  return (out)
}

#' Widens prediction dataframe (useful when 2+ prediction variables are generated)
#'
#' @param cmdstan_preds a cmdstan summary with extracted prediction variables
#'
#' @return
#' @export
#'
#' @examples
widen_preds <- function(cmdstan_preds) {
  # out = cmdstan_preds %>%
    mutate(var_type = sub("\\[\\b.*",'',variable)) %>%
    select(-variable) %>%
    pivot_wider(names_from = var_type, values_from = mean)
}

#XX combine into 1 func?
