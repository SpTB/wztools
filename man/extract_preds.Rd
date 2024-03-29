% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stan_predictions.R, R/stan_summaries2.R
\name{extract_preds}
\alias{extract_preds}
\title{Extract simulated variables from the generated quantities block}
\usage{
extract_preds(cmdstan_summary, preds = c("d_pred", "r_pred"))

extract_preds(cmdstan_summary, preds = c("d_pred", "r_pred"))
}
\arguments{
\item{cmdstan_summary}{a cmdstan summary object}

\item{preds}{a list of predictor names}
}
\value{
a dataframe containing predictions
}
\description{
The summary object is assumed to to be 2D (subjID, trial) or 3D (subjID, game, trial), but should work for 1D -> in 1D case (leaves only trials as the grouping var)
}
