## code to prepare `DATASET` dataset goes here

usethis::use_data(DATASET, overwrite = TRUE)

set.seed(1)

#set design
#nsubj=10
conds = c('Easy', 'Diff')
ntrial=25 #trials per condition
acc_diff = 0.65 #mean accuracy in the difficult condition
acc_easy = 0.90 #mean accuracy in the easy condition
rt_diff = 2.15 #mean RT by condition
rt_easy = 1.35
rt_sd = 0.5
#create grouping columns
cond  = rep(conds, each=ntrial)
trial = rep(1:ntrial, length(conds))

#outcomes
choice = c(rbinom(n=ntrial, prob =acc_easy, size=1), #choosing the better option / accuracy
           rbinom(n=ntrial, prob =acc_diff, size=1))
rt=c(truncnorm::rtruncnorm(n=ntrial, a=0.2, b=3, mean=rt_easy, sd=rt_sd), #rt
     truncnorm::rtruncnorm(n=ntrial, a=0.2, b=3, mean=rt_diff, sd=rt_sd))

single_subj_sim = tibble::tibble(
  cond,
  trial,
  choice,
  rt
)

usethis::use_data(single_subj_sim, compress='xz')
