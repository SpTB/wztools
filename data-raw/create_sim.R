## code to prepare `DATASET` dataset goes here

usethis::use_data(DATASET, overwrite = TRUE)

set.seed(1)

#set design
nsubj=10
conds = c('Easy', 'Diff')
ntrial=25 #trials per condition
acc_diff = 0.65 #mean accuracy in the difficult condition
acc_easy = 0.90 #mean accuracy in the easy condition
acc_diff_beta_pars = estBetaParams(acc_diff,std=.1)
acc_easy_beta_pars = estBetaParams(acc_easy,std=.1)
rt_diff = 2.15 #mean RT by condition
rt_easy = 1.35
rt_sd_between = 0.2
rt_sd_within = 0.5

#create grouping columns
cond  = rep(conds, each=ntrial)
trial = rep(1:ntrial, length(conds))

#subject-lvl vars
acc_easys = rbeta(n=nsubj, shape1=acc_easy_beta_pars[[1]], shape2=acc_easy_beta_pars[[2]])
acc_diffs = rbeta(n=nsubj, shape1=acc_diff_beta_pars[[1]], shape2=acc_diff_beta_pars[[2]])
rt_easys = truncnorm::rtruncnorm(n=nsubj, a=0.2, b=3, mean=rt_easy, sd=rt_sd_between)
rt_diffs = truncnorm::rtruncnorm(n=nsubj, a=0.2, b=3, mean=rt_diff, sd=rt_sd_between)

#outcomes
sim = NULL
for (s in seq_along(1:nsubj)) {
  choice = c(rbinom(n=ntrial, prob =acc_easys[s], size=1), #choosing the better option / accuracy
             rbinom(n=ntrial, prob =acc_diffs[s], size=1))
  rt=c(truncnorm::rtruncnorm(n=ntrial, a=0.2, b=3, mean=rt_easys[s], sd=rt_sd_within), #rt
       truncnorm::rtruncnorm(n=ntrial, a=0.2, b=3, mean=rt_diffs[s], sd=rt_sd_within))

  subj = tibble::tibble(
    subjID = rep(s, length(choice)),
    cond,
    trial,
    choice,
    rt
  )
  sim=rbind(sim,subj)
}


usethis::use_data(sim, compress='xz')
