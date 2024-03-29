% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/beta_distribution.R
\name{draw_from_beta_hypers}
\alias{draw_from_beta_hypers}
\title{Draw individual parameters from beta-transformed hyper-parameter}
\usage{
draw_from_beta_hypers(native_mu, sigma, native_range, nsub)
}
\arguments{
\item{native_mu}{Mean of the hyper-parameter in native scale}

\item{sigma}{Std of the hyper-parameter in beta-scale}

\item{native_range}{range of the native scale (e.g. c(-5,5))}

\item{nsub}{number of subjects}
}
\value{
Individual parameter vector (in native scale)
}
\description{
Draw individual parameters from beta-transformed hyper-parameter
}
