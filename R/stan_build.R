stan_bounds <- function(var_names) {
  up_bounds = purrr::map_chr(var_names, ~paste0("\n\treal ub_",.,";")) %>% paste(., collapse='')
  down_bounds = purrr::map_chr(var_names, ~paste0("\n\treal lb_",.,";")) %>% paste(., collapse='')
  gamA = purrr::map_chr(var_names, ~paste0("\n\tvector[2] gamA_",.,";")) %>% paste(., collapse='')
  gamB = purrr::map_chr(var_names, ~paste0("\n\tvector[2] gamB_",.,";")) %>% paste(., collapse='')
  data = paste0("data {\n\tint<lower=1>numSubj; \n\tint<lower=0>numTrials;\n", up_bounds, down_bounds, gamA, gamB, "\n}") %>% paste(., collapse='')
  #cat(data, sep="\n")
}

stan_parameters <-function(var_names) {
  alphas = purrr::map_chr(var_names, ~paste0("\n\treal <lower=0> a_",.,"_raw;")) %>% paste(., collapse='')
  betas = purrr::map_chr(var_names, ~paste0("\n\treal <lower=0> b_",.,"_raw;"))  %>% paste(., collapse='')
  inds = purrr::map_chr(var_names, ~paste0("\n\tarray[numSubj] real <lower=0, upper=1> ",.,"_raw;"))  %>% paste(., collapse='')
  pars = paste0("\nparameters{\n", alphas, "\n\t", betas, "\n", inds, "\n}")
  #cat(pars)
}

stan_transformed_parameters <-function(var_names) {
  alphas = purrr::map_chr(var_names, ~paste0("\n\treal <lower=1> a_",.,";")) %>% paste(., collapse='')
  betas  = purrr::map_chr(var_names, ~paste0("\n\treal <lower=1> b_",.,";"))  %>% paste(., collapse='')
  mus    = purrr::map_chr(var_names, ~paste0("\n\treal <lower=lb_",.,",upper=ub_",.,"> mu_",.,";"))  %>% paste(., collapse='')
  inds   = purrr::map_chr(var_names, ~paste0("\n\tarray[numSubj] real <lower=lb_",.,",upper=ub_",.,"> ",.,";"))  %>% paste(., collapse='')
  a_trans= purrr::map_chr(var_names, ~paste0("\n\ta_",.,"= a_",., "_raw+1;")) %>% paste(., collapse='')
  b_trans= purrr::map_chr(var_names, ~paste0("\n\tb_",.,"= b_",., "_raw+1;")) %>% paste(., collapse='')
  mu_trans= purrr::map_chr(var_names, ~paste0("\n\tmu_",.,"= a_",.,"/(a_",.,"+b_",.,") *(ub_",.,"-lb_",.,")+lb_",.,";")) %>% paste(., collapse='')

  ind_trans_inloop = purrr::map_chr(var_names, ~paste0("\n\t\t",.,"[s] =",.,"_raw[s] * (ub_",.,"-lb_",.,") + lb_",.,";")) %>% paste(., collapse='')
  ind_trans = paste('\n\tfor (s in 1:numSubj) {', ind_trans_inloop, "\n\t}")
  tpars =  paste0("\ntransformed parameters{\n", alphas, "\n", betas, "\n", mus, "\n", inds, "\n", a_trans,"\n", b_trans,"\n", mu_trans,"\n", ind_trans,"\n}")
}

stan_model <-function(var_names) {
  alphas = purrr::map_chr(var_names, ~paste0("\n\ta_",.,"_raw ~ gamma(gamA_",.,"[1],gamA_",.,"[2]);")) %>% paste(., collapse='')
  betas  = purrr::map_chr(var_names, ~paste0("\n\tb_",.,"_raw ~ gamma(gamB_",.,"[1],gamB_",.,"[2]);")) %>% paste(., collapse='')
  inds_inloop = purrr::map_chr(var_names, ~paste0("\n\t\t",.,"_raw[s] ~ beta(a_",.,", b_",.,");"))%>% paste(., collapse='')
  inds = paste('\n\tfor (s in 1:numSubj) {', inds_inloop, "\n\t}")
  out=paste0("\nmodel{\n",alphas, "\n",betas, "\n",inds,"\n}")
}

#' Builds a stan-model skeleton, specifying priors, their ranges, and shape, based on the idea of sampling from a bounded continuous space
#'
#' @param var_names list of parameter names
#'
#' @return
#' @export
#'
#' @examples
make_stan_skeleton <-function(var_names) {
  data = stan_bounds(var_names)
  pars = stan_parameters(var_names)
  tpars =stan_transformed_parameters(var_names)
  model = stan_model(var_names)
  out =paste(data ,pars, tpars, model)
}
