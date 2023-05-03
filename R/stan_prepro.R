##ALL FUNCTIONS THAT INTERACT WITH STAN OR STAN-DERIVED OBJECTS

compile_model <- function(model_file) {
  cmdstan_model(model_file)
  return (model_file)
}

#' Add ranges to stan list
#'
#' @param stan_list
#' @param ranges
#'
#' @return
#' @export
#'
#' @examples
add_ranges <- function(stan_list,ranges) {
  for (i in 1:length(ranges)) {
    if ('range' %in% names(ranges[[i]]))  {
      stan_list[[paste0('lb_',names(ranges)[i])]] = ranges[[i]]$range[1]
      stan_list[[paste0('ub_',names(ranges)[i])]] = ranges[[i]]$range[2]
    }
  }
  return (stan_list)
}

#' add fixed pars to stan list
#'
#' @param stan_list
#' @param fixed_pars
#' @param model_struct_list
#'
#' @return
#' @export
#'
#' @examples
add_fixed_pars <-function(stan_list, fixed_pars, model_struct_list) {
  for (par in fixed_pars) {
    stan_list[[par]] = model_struct_list[[par]]$fixed_value
  }
  return (stan_list)
}

#' Add gamma pars from model_struct_list to stan_list
#'
#' @param stan_list
#' @param model_struct_list
#'
#' @return
#' @export
#'
#' @examples
add_gammas <- function(stan_list, model_struct_list) {
  for (i in 1:length(model_struct_list)) {
    stan_list[[paste0('gamA_',names(model_struct_list)[i])]]=model_struct_list[[i]]$gamma_parsA
    stan_list[[paste0('gamB_',names(model_struct_list)[i])]]=model_struct_list[[i]]$gamma_parsB
  }
  return (stan_list)
}


#' Making a stan-compatible data list
#'
#' @param dat original dataframe
#' @param var_names output variables (choice, rt, outcome, etc...)
#' @param grouping_vars grouping variables (subjID, games, trials)
#' @param grouping_vars_stan_names specifying the grouping variable stan-names
#' @param hierarchical whether the dataset is multi-subject (T) or not (F)
#' @param ssm_prepro whether to include lower RT bounds for sequential sampling models input
#'
#' @return
#' @import dplyr
#' @export
#'
#' @examples
make_stan_list <- function(dat, model_struct_list=NULL, fixed_pars=NULL, var_names,  grouping_vars = c('subjID', 'game', 'trial'), grouping_vars_stan_names=c('numSubj', 'numGames', 'numTrials'), hierarchical = T, ssm_prepro=F,
                           add_ranges=T,add_fixed_pars=T, add_gammas=T) {
  # transforms a data df into a list for stan

  #remove unnecessary columns, like parameter values, etc:
  dat = dat %>%
    select_if(names(.) %in% c(var_names, grouping_vars))

  #case: e1 c('choice', 'choseA', 'outcome', 'free', 'cond','mixed','game','trial','subjID','rated','rating', 'rt'))

  #divide variables by their role
  names(grouping_vars) = grouping_vars_stan_names

  #if (!hierarchical) { #remove subject grouping if only one
  #  grouping_vars = grouping_vars[2:length(grouping_vars)]
  #}

  looping_vars = names(dat)[!(names(dat) %in% grouping_vars)] #output vars


  #get meta pars (how many participants, games, choices per game)
  stan_data<-list()
  for(col in grouping_vars) {
    out_name = names(grouping_vars)[grouping_vars == col]
    stan_data[[out_name]] = max(dat[[col]])
  }

  #if rt in data, preprocess rt vars necessary for ddm
  if ('rt' %in% names(dat) & ssm_prepro) {
    stan_data$RTbound = min(dat$rt[dat$rt>-1])
    if (hierarchical) {
      stan_data$minRT = dat %>% filter(rt>-1) %>% group_by(subjID) %>% summarise(min(rt))  %>% tibble::deframe()
    }
  }

  for (col in looping_vars) {
    stan_data[[col]] =  dat %>%
      group_by(across(all_of(unname(grouping_vars)))) %>%  #subjID, game, trial
      select(col) %>%
      tidyr::pivot_wider(names_from = trial, values_from=col)# %>%
     # arrange(subjID)
    #ungroup()
    if ('subjID' %in% grouping_vars)  stan_data[[col]] = stan_data[[col]] %>% arrange(subjID)
  }



  #hierarchical
  if (hierarchical) {

    #this part was to make an rt+choice double array for LBA, but not useful anymore
    # if (join_rt_choice) {
    #   choice = split (stan_data$choice, stan_data$choice$subjID) %>% #split by subj (separate lists)
    #     lapply( FUN = function(x) x[,3:ncol(x)]) %>%  #remove grouping
    #     lapply(as_vector) %>% #reduce dims
    #     abind::abind(along=2) # list to array
    #   choice = choice[choice>0] #remove missing data (due to RTs)
    #   rt = split (stan_data$rt, stan_data$rt$subjID) %>% #split by subj (separate lists)
    #     lapply( FUN = function(x) x[,3:ncol(x)]) %>%  #remove grouping
    #     lapply(as_vector) %>% #reduce dims
    #     abind::abind(along=2) # list to array
    #   rt = rt[rt>0] #remove missing data (due to RTs)
    #   stan_data$RT = abind::abind(rt, choice, along=3) #join
    # }

    for (nam in looping_vars) {
      stan_data[[nam]] = split (stan_data[[nam]], stan_data[[nam]]$subjID)
      stan_data[[nam]] = lapply(stan_data[[nam]], FUN = function(x) x[,3:ncol(x)]) # remove grouping cols
      stan_data[[nam]] = abind::abind(stan_data[[nam]], along=3) # create array (dims: games, trials, subjs)
      stan_data[[nam]] = aperm(stan_data[[nam]], c(3,1,2)) # rearrange dims: subjID, games, trials
    }

  } else { #single subj: remove grouping game column
    for (nam in looping_vars) {
      stan_data[[nam]] =  stan_data[[nam]][,2:ncol(stan_data[[nam]])] %>% as.matrix()
    }
    #stan_data$outcome  = stan_data$outcome[,2:ncol(stan_data$outcome)] %>% as.matrix()
  }

  if (add_ranges)     stan_data=add_ranges(stan_data,model_struct_list)
  if (add_fixed_pars) stan_data=add_fixed_pars(stan_data, fixed_pars, model_struct_list)
  if (add_gammas)     stan_data=add_gammas(stan_data, model_struct_list)

  return (stan_data)
}



