### RECOVERY TEMPLATE

library(targets)
library(tarchetypes)
library(stantargets)
wztools::setOptions()
wztools::sourceAll(path = 'XX/1.funcs', pattern = '_func')

tar_option_set(packages = c('tidyverse', 'wztools', 'cmdstanr'))#'obtools',

### RECOVERY ###
list(
  tar_target(name=settings,
             command = list(
                            #exp_num = x,
                            nsim=10,
                            nsubj=50,
                            par_names = c(),
                            scale_vector = c(),
                            shift_vector = c(),
                            fixed_pars = list()
  ))


  #load raw alphas and betas of winning model
  #!! Note 1: should only include parameters that are in settings$par_names!!
  ,
  tar_target(name=mu_alphas,
             command = read.csv('mu_alphas.csv'),
             # cue = tar_cue('always')
  )
  ,
  tar_target(name=mu_betas,
             command = read.csv('mu_betas.csv'),
             # cue = tar_cue('always')
  )
  ,

  tar_target(name=sim_mus, #DIM: 10 x 39 (NSIM x (pars*2+1)) )
             #cue = tar_cue('always'),
             command = sim_raw_pars_from_ab(settings_list = settings,
                                            alpha_df = mu_alphas,
                                            beta_df = mu_betas,
                                            #nsim = 2,
                                            single_df=T,
                                            seed = 2),
  )

  # # exclude unwanted columns (only if Note 1 not true, and we have too many)
  # ,tar_target(name=sim_mus,
  #             command=sim_mus_pre %>%
  #               select(matches(paste(choice_settings$par_names, collapse = "|"))) %>%
  #               mutate(sim_num=1:nrow(sim_mus_pre))
  # )
  # ,
  ,
  tar_target(name = sim_mus_native,
             command = raw_to_native(sim_mus,
                                     prefix = 'mu_',
                                     scale_vector=settings$scale_vector,
                                     shift_vector=settings$shift_vector))
  #
  # #
  # # #
  # # #  #split to groups by simulation number
  ,
  tar_group_by(name=grouped_sim_mus,
               command = sim_mus,
               sim_num)
  ,
  tar_target(name=sim_inds, #DIM: 500 x 21 (19 pars, sim_num, subjID)
             command=sim_raw_inds_from_ab(settings_list = settings,
                                          mus_df = grouped_sim_mus,
                                          nsim=1,
                                          suffix='_raw',
                                          nsubj=settings$nsub,
                                          seed=2),
             # cue = tar_cue('always'),
             pattern=map(grouped_sim_mus))
  # #
  ,
  tar_target(name=stan_list_gen,
             command = xxExp_specific_gen_func(sim_inds),
             pattern=map(sim_inds))
  #
  #  #add fixed pars
  ,
  tar_target(name=stan_list_gen_fixed,
             command = modifyList(stan_list_gen, fix_list),
             pattern = map(stan_list_gen))
  ,
  tar_target(name=gq_df,
             command = fit_model(stan_data=stan_list_gen_fixed,
                                 model_file='',
                                 summary_only=T,
                                 chains=1,
                                 parallel_chains = 1,
                                 fixed_param=T,
                                 iter_sampling = 1,
                                 refresh = 100
             ),
             #cue=tar_cue(mode='never'),
             pattern = map(stan_list_gen_fixed))
  #
  #  #  #predicted choices & judgments (exp specific, of course..)
  ,
  tar_target(name=dpred,
             command = extract_dpred(gq_df),   #_summary_E2_zoib_gq
             pattern=map(gq_df))
  ,
  tar_target(name=jpred,
             command = extract_jpred(gq_df),
             pattern=map(gq_df))
  #   ,

  ### RECOVERY ###
  # modify list (if necessary)
  ,
  tar_target(name=stan_list_sim_data,
             command = modify_stan_list_for_recov(stan_list=stan_list_gen_fixed,
                                                  dpred=dpred,
                                                  jpred=jpred,
                                                  nsubj=settings$nsubj
             ),
             pattern = map(stan_list_gen_fixed, dpred, jpred))
  ,
  #recovery
  tar_target(name=mrecov,
             command = fit_model(stan_data=stan_list_sim_data,
                                 model_file = '',
                                 summary_only=T,
                                 parallel_chains = 4,
                                 chains = 4,
                                 iter_warmup = 500,
                                 iter_sampling = 500,
                                 max_treedepth = 10,
                                 adapt_delta = .95,
                                 refresh = 10
             ),
             pattern = map(stan_list_sim_data),
             #cue=tar_cue(mode='never')
  )
)
