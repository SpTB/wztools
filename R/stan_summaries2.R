get_mus2 <- function(stan_fit_summary) {
  mus = stan_fit_summary |>
    filter(str_detect(variable, 'mu') & !str_detect(variable, 'raw'))
  return(mus)
}

get_inds2<-function(stan_fit_summary, ind_pars=NULL) {
  #extract a list of individual parameters followed by '[x]'
  search_vect = map_chr(ind_pars, ~paste0(.x, '\\['))

  inds = stan_fit_summary %>%
    filter(str_detect(variable,    paste(search_vect,collapse = '|'))) %>%
    filter(!str_detect(variable, 'log_lik')) %>%
    filter(!str_detect(variable, 'y_pred')) %>%
    mutate(var_type = sub("\\[\\b.*",'',variable),
           subjID = as.numeric(str_extract(variable, "\\d+")))

  return(inds)
}