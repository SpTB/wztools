### POSTERIOR PREDICTIONS /COUNTERFACTUAL SIMULATIONS TEMPLATE

library(targets)
library(tarchetypes)
library(stantargets)
wztools::setOptions()
wztools::sourceAll(path = 'XX/1.funcs', pattern = '_func')
tar_option_set(packages = c('tidyverse', 'wztools', 'cmdstanr'))#'obtools',


list(
  tar_target(name=settings,
             #  cue = tar_cue('always'),
             command = list(
               exp_num=x,
               nsim=10,
               nsubj=50,
               par_names = c(),
               fixed_pars = list(),
             ))

  ###### ###
  # LOADING
  #########
  ,
  tar_target(name=dat,
             command = read_csv('/path/to/dat.csv')#,
             #           cue=tar_cue('never')
  )

  #get best mus
  ,
  tar_target(name=mus,
             command = readRDS("../../path/to/mus")
  )
  # simple way through mus, but can also go the recov way here...(using alphas_df/betas_df)
  ,
  tar_target(name=sim_inds_choice,
             command = sim_inds(mus, settings$nsubj)
             )

  #choice model stan list
  ,
  tar_target(name=stan_list_full,
             command = readRDS("../../2.fitting/targets/objects/stan_list_full")
  )
  ,
  tar_target(name=stan_list_gen,
             command = modify_stan_list_for_gq_xx(
               stan_list = stan_list_full,
               sim_inds=sim_inds_choice,
               nsubj=choice_settings$nsubj,
               exp_num=choice_settings$exp_num,
               add_raw=F #do or don't add '_raw' at the end of par names
             )
  )



  # # # # # # ############
  # # # # # # # Generate Choices
  # # # # # # ##############

  ,
  tar_target(name=gq1,
             # cue=tar_cue(mode='always'),
             command = fit_model(stan_data=stan_list_gen,
                                 model_file='stan_models/mxx_gq.stan',
                                 summary_only=T,
                                 chains=1,
                                 parallel_chains = 1,
                                 fixed_param=T,
                                 #variables = c('d_pred', 'j_pred'),
                                 iter_warmup = 1,
                                 iter_sampling = 1,
                                 refresh = 100,
                                 seed=1
             ),
  )





  #predicted choices
  ,
  tar_target(name=dpreds1,
             # cue=tar_cue(mode='always'),
             command = extract_preds(gq1, preds = c('d_pred', 'outcome')),
  )


  # #combined with data
  ,tar_target(name=d1,
             #cue=tar_cue(mode='always'),
             command = dpreds1 %>%
               mutate(var_type=rep(c('dpred', 'outcome'), nrow(dpreds1)/2)) %>%
               select(-variable) %>%
               pivot_wider(names_from=var_type, values_from=mean) %>%
               right_join(dat,by=c('subjID', 'game', 'trial'))
  )
)
