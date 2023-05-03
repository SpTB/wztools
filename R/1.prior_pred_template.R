# ### STEP 6:
# # adding confidence ratings
#
# library(targets)
# library(tarchetypes)
# library(stantargets)
# wztools::setOptions()
# wztools::sourceAll(path = 'project-XX/1_functions', pattern = '_func')
# tar_option_set(packages = c('XXtools','tidyverse', 'wztools', 'cmdstanr'))
#
# list(
#
#   ### 1. Setup ###
#
#   # specify task a) structure-related parameters, and b) non-modelled parameters for simulation
#   tar_target(name = exp_struct_list,
#              command = create_exp2_setup()
#   )
#   ,
#   #specify group level parameters (mus)
#   tar_target(name = mu_list,
#              command = list(mu_XX1        =0,
#                             #setting multiple values is allowed (will result in automatic branching)
#                             mu_XX2  =base::sample(c(-1.00, 0.0, 1.00),2),
#              )
#   )
#   ,
#
#   #list of ranges
#   tar_target(name = range_list,
#              command= list(mu_XX1 = c(-5,5),
#                            # to fix the parameter, set min & max to the fixed value, e.g.,:
#                            mu_XX2 = c(1,1) #
#              )
#   )
#   ,
#   #list of group-level variability around the mean (beta-space)
#   tar_target(name = sigma_list,
#              command =  list(mu_XX1 = .10, #default variability
#                              mu_XX2 = .05 #decreased variability
#              )
#   )
#   ,
#   #list specifying fixed parameters (-1: NON-FIXED; any other value: fixed to that value)
#   tar_target(name = fixed_pars,
#              command = list(fix_XX1 = -1, #not fixed
#                             fix_XX2 = 1 #fix value has to correspond to the one in range_list
#                             )
#   )
#   ,
#
#
#   ### 2. Simulate individual pars and task skeleton ###
#
#   #simulate individual parameters
#   tar_target(name=ind_pars,
#              command = sim_ind_pars_multi(exp_struct_list, mu_list, sigma_list, range_list))
#   ,
#   #simulate task skeleton (with embedded parameter values as col names!)
#   #(added '_true' to par name, e.g. XX_true)
#   tar_target(name=empty_task_df,
#              command = sim_taskXX_empty_multi(exp_struct_list, mu_list, ind_pars)
#   )
#   ,
#   #group by simulation
#   tar_group_by(name = branching,
#                command = empty_task_df,
#                sim_num
#   ),
#   tar_target(name = empty_df_grouped,
#              command=branching,
#              pattern=map(branching)
#   )
#   ,
#
#
#
#   ### 3. Stan list conversion ###
#
#   # convert to stan-list (one for each simulation)
#   tar_target(name = stan_list_skeleton,
#              command = make_stan_list(dat=empty_df_grouped,
#                                       var_names=c('outcomeA','outcomeB','reward', 'observation', 'is_rating'),
#                                       grouping_vars_stan_names=c('numSubj', 'numGames', 'numTrials')
#              ),
#              pattern = map(empty_df_grouped)
#
#   ),#extract individual parameter list
#   tar_target(name = gen_pars_list,
#              command = pars_to_stan(empty_df_grouped, mu_list),
#              pattern = map(empty_df_grouped)
#   )
#   ,# combine
#   tar_target(name = stan_list_gen_prep,
#              command = c(stan_list_skeleton, gen_pars_list),
#              pattern = map(stan_list_skeleton, gen_pars_list)
#   )
#   ,#format for specific experiment: change reward dimentionality
#   tar_target(name = stan_list_gen,
#              command = format_stan_list_gen_XX(stan_list_gen_prep),
#              pattern = map(stan_list_gen_prep)
#   )
#   ,
#
#
#
#   ### 4. SIMULATE ###
#   tar_target(name = stan_sim,
#              command =fit_model_gen(stan_data=stan_list_gen,
#                                     model_file= c(paste0(getwd(),'/stan/stepXX_gen.stan')),#'D:/Observing_bandits/2_models/exp2/1.prior_predictive_checks/Step5/stan/step5_gen.stan',
#                                     summary_only=T
#              ),
#              pattern = map(stan_list_gen)
#   )
#   ,
#
#   #get back simulated choices and ratings
#   tar_target(name=pred_long,
#              command = extract_preds(stan_sim, c('d_pred', 'r_pred')),   #_summary_E2_zoib_gq
#              pattern=map(stan_sim)
#   )
#   ,
#   # # wide data
#   tar_target(name=pred,
#              command = widen_preds(pred_long),
#              pattern=map(pred_long)
#   )
#   ,
#   # join with the skeleton df (for exploration)
#   tar_target(name = pred_full,
#              command = left_join(empty_df_grouped, pred),
#              pattern = map(empty_df_grouped, pred)
#   )
#   ,
#   ## REPORT 1 CAN GO HERE
#
#
#
#   ### 5. FIT ####
#   # predictions to list
#   tar_target(name = rec_list_add,
#              command = make_stan_list(dat = pred,
#                                       var_names=c('d_pred','r_pred')),
#              pattern = map(pred))
#   ,
#   #recover list (combine 3 lists: skeleton,  fixed parameters and predictions)
#   tar_target(rec_list_prep,
#              command = modifyList(c(stan_list_gen, fixed_pars),rec_list_add),
#              pattern = map(stan_list_gen, rec_list_add))
#   ,
#   #prep for recovery: change names dpred -> choice; rpred->rating
#   tar_target(name = rec_list,
#              command = format_stan_list_rec_step6(rec_list_prep),
#              pattern = map(rec_list_prep))
#   ,
#
#   tar_target(name = mrec,
#              command = fit_model(
#                stan_data = rec_list,
#                model_file= c(paste0(getwd(),'/stan/step6.stan')),
#                summary_only=T,
#                iter_warmup = 500,
#                iter_sampling=500,
#                chains = 2,
#                parallel_chains = 2,
#                refresh = 10
#              ),
#              pattern = map(rec_list)
#   )
#   # ,
#
#   # ### 6. EXTRACT VALUES ###
#   #
#   # #   #  get mus
#   # tar_target(name = mu_fit,
#   #            command = get_mus2(stan_fit_summary=mrec,
#   #                              mu_list=mu_list,
#   #                              model_num = 1)#length(model_files) from target above
#   # ),
#   # #
#   # # get individual
#   # tar_target(name = ind_fit,
#   #            command = get_inds_temp(stan_fit_summary=recovered_fit,
#   #                               ind_pars=ind_pars,
#   #                               model_num = 1)
#   # )
#
#   ### CURRENT END
#
#
#
#
#
#
#
#
#
#   #   #
#   #   # report 2
#   #   tar_render(name = report_fit, "Step5_fit_report.Rmd"
#   #   )
#
# )
#
