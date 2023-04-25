#' Title
#'
#' @desription Draw a parameter mean from a re-scaled beta distribution defined by alpha and beta drawn from gamma distributions
#'
#' @param gamma_parsA gamma priors for alpha parameter of the Beta distribution
#' @param gamma_parsB gamma priors for beta parameter of the Beta distribution
#' @param shift
#' @param scale
#' @param nsim
#'
#' @return
#' @export
#'
#' @examples
draw_mu_from_gamma <-function(gamma_parsA=c(1,1), gamma_parsB=c(1,1), shift=0, scale=1, nsim) {
  alpha = rgamma(n=nsim, gamma_parsA[1], gamma_parsA[2])+1
  beta = rgamma(n=nsim, gamma_parsB[1], gamma_parsB[2])+1
  mu_beta = alpha/(alpha+beta) * scale + shift
  return (mu_beta)
} # ++

#' Title
#'
#' @param model_struct_list Model struture list
#' @param prior_dist prior distribution (currently works for 'gamma' (default), 'beta' and 'gauss')
#' @param nsim number of simulations
#' @param seed
#'
#' @return
#' @export
#'
#' @examples
sim_mus2 <-function(model_struct_list, prior_dist='gamma', nsim, seed=NULL) {

  if (!is.null(seed)) set.seed(seed)
  out=list()

  prior_comb = model_struct_list
  for (i in seq_along(1:length(prior_comb))) {
    namei = paste0('mu_', names(prior_comb[i]))
    #out[[names(prior_comb[i])]] = NULL
    if ('fixed_value' %in% names(prior_comb[[i]])) {
      out[[namei]]$value = prior_comb[[i]]$fixed_value
      out[[namei]]$fixed = T
    }
    else {
      if (prior_dist=='beta') {
        out[[namei]] = wztools::draw_from_beta_hypers(native_mu   =prior_comb[[i]]$mu,
                                                      sigma       =prior_comb[[i]]$sigma,
                                                      native_range=prior_comb[[i]]$range,
                                                      nsub=nsim)
        out[[namei]]$fixed = F
      } else if (prior_dist=='gamma') {

        out[[namei]]$value = draw_mu_from_gamma(gamma_parsA=prior_comb[[i]]$gamma_parsA,
                                                gamma_parsB=prior_comb[[i]]$gamma_parsB,
                                                shift =prior_comb[[i]]$range[1],
                                                scale = prior_comb[[i]]$range[2]-prior_comb[[i]]$range[1],
                                                nsim=nsim)
        out[[namei]]$fixed = F


      } else if (prior_dist=='gauss') {
        out[[namei]] = rnorm(mean =prior_comb[[i]]$mu,
                             sd   =prior_comb[[i]]$sd,
                             n=nsim)
        out[[namei]]$fixed = F
      }
    } # if/else fixed val
  } #parameter loop
  return (out)
}

#### 4. RECOVERY ##############

#' Simulates raw group parameter values (in beta space) from alpha/beta dataframes
#' @description Parameter recovery: for drawing group-level parameter values
#' @param settings_list has to contain $nsim field with number of simulations
#' @param alpha_df N-by-p df, where p are the parameter names (in the form 'a_x_raw')
#' @param beta_df same for beta values ('b_x_raw' colnames)
#' @param out_pars output parametrization: 'beta' returns alpha & beta values; otherwise: mean + sd
#' @param single_df output type: TRUE returns a single dataframe; FALSE a list of 2 elements
#' @param seed
#'
#' @return
#' @export
#'
#' @examples
sim_raw_pars_from_ab <- function(settings_list, alpha_df, beta_df, out_pars = 'beta', single_df=T, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  nsim=settings_list$nsim

  as_samp = alpha_df[1:nsim,]
  bs_samp = beta_df[1:nsim,]
  for (col in 1:ncol(alpha_df)) {
    samp = sample(1:nrow(alpha_df), nsim) #sample rows (crucial the sample is the same for both dfs!)
    as_samp[,col] = alpha_df[samp,col]
    bs_samp[,col] = beta_df[samp,col]
  }
  out = list()
  if (out_pars == 'beta') {
    out[[1]] = as_samp #alpha pars
    out[[2]] = bs_samp #beta pars
  } else {
    out[[1]] = as_samp/(as_samp+b_samp) #beta mean
    out[[2]] = sqrt((as_samp*bs_samp)/((as_samp+bs_samp)^2*(as_samp+bs_samp+1))) #beta sd
  }
  if (single_df){
    out = bind_cols(out[[1]], out[[2]])
    out$sim_num = 1:nrow(out)
  }
  return (out)
}


#' simulates raw individual parameter values (in beta space) from alpha/beta dataframes
#'
#' @param mus_df
#' @param out_pars
#' @param nsim
#' @param nsubj
#' @param starting_string string at the start of each column name. Defaults to '' (empty), but sometimes 'mu_' might work
#' @param seed
#'
#' @return
#' @export
#'
#' @examples
sim_raw_inds_from_ab <-function(mus_df, out_pars = 'beta', nsim, nsubj, starting_string='',seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  #column starting prefixes:
  alpha_prefix = paste0(starting_string, 'a_')
  beta_prefix = paste0(starting_string, 'b_')

  alpha_df = mus_df %>% select(starts_with(alpha_prefix))
  beta_df = mus_df %>% select(starts_with(beta_prefix))

  par_names = substr(names(alpha_df),nchar(alpha_prefix)+1, nchar(names(alpha_df)))

  for (sim in 1:nsim) { #for each simulation
    for (par in 1:ncol(alpha_df)){ #for each parameter
      parx = rbeta(nsubj, pull(alpha_df[sim,par]), pull(beta_df[sim,par]))
      if (par==1) simx = parx else simx = bind_cols(simx, parx)
    }
    names(simx) = par_names
    simx$sim_num = sim
    simx$subjID = 1:nsubj
    if (sim==1) out=simx else out = bind_rows(out, simx)
  }
  return(out)
}
