# FITTING TEMPLATE

library(targets)
library(tarchetypes)
library(stantargets)
wztools::setOptions()
wztools::sourceAll(path = 'XX/1.funcs', pattern = '_func')
tar_option_set(packages = c('tidyverse', 'wztools', 'cmdstanr'))

list(
  tar_target(name = settings,
             command = list(
                 par_names=c(),
                 range_list=list(),
                 fix_list = list()
             ))

  ,tar_target(name=fixed_pars,
              command = make_fixed_pars_list(settings$range_list, settings$fix_list)) ## XX!!!

  # load data...
  ,tar_target(name=data_list,
             command = read_csv('/path/to/data.csv')
  )

  #stan_list
  # xx might branch somewhere here, if many models...
  ,tar_target(name=stan_list,
             command = make_stan_list_xx(dat))

  # #add fixed pars
  ,tar_target(name=stan_list_full,
             command = modifyList(stan_list, fixed_pars))

  ,
  tar_stan_mcmc(
   # cue = tar_cue('never'),
    name = xx,
    stan_files ='/path/to/stan_model.stan',
    data = stan_list_full,
    parallel_chains = 4,
    chains = 4,
    iter_warmup = 500,
    iter_sampling = 500,
    refresh = 10
  )

 #####
 ###loo/waic scores ###
 ######
 # XX add weights
 # this is a version where models not branching, but manual
 ,
 tar_target(name = loo_list,
            command = list(
              m1 = calc_loo_new(m1),
              m2 = calc_loo_new(m2),
            )
 )
 ,
 tar_target(name=waic_df,
            command = bind_rows(
              calc_waic_new(m1, 'm1'),
              calc_waic_new(m2, 'm2'),
            )
 )

 #arrange in a df
 ,
 tar_target(name = gof,
            #cue=tar_cue('never'),
            command = loos_to_df_new(loo_list) %>%
              left_join(waic_df,by = 'model') %>%
              select(model,
                     loo = Estimate_looic,
                     loo_SE = SE_looic,
                     waic = WAIC)
 )

  #############
  #####SUMMARIES & PREDS
  ############
 ,
 tar_target(name=mus,
            #   cue = tar_cue('always'),
            command = get_mus2(stan_fit_summary = mxx_summary)
 )
 ,
 tar_target(name=inds,
            command = get_inds2(stan_fit_summary = mxx_summary,
                                ind_pars = substr(mus$variable, 4, nchar(mus$variable)))
 )

 ,tar_target(name=dpred,
             command = extract_preds( mxx_summary,
                                      preds = 'd_pred'))
  #######
  # PREP FOR RECOV
  ########
  #
   ,
   tar_target(name=mu_alphas,
              command = drawsxx %>%
                select(starts_with('mu_a_')) %>%
                select(!contains('_raw') %>%
                write_csv(file = '../recover/mu_alphas.csv'))
   )

   ,tar_target(name=mu_betas,
              command = drawsxx %>%
                select(starts_with('mu_b_')) %>%
                select(!contains('_raw') %>%
                write_csv(file = '../recover/mu_betas.csv'))
   )
)



