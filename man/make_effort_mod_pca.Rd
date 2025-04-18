% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/effort.R
\name{make_effort_mod_pca}
\alias{make_effort_mod_pca}
\title{Model the effect of principal components on taxa richness.}
\usage{
make_effort_mod_pca(
  df,
  env_prcomp,
  context = "cell",
  cores = 4,
  response = "sr",
  response_min_thresh = 2,
  threshold_lo = 0.05/2,
  threshold_hi = 0.05/2,
  effort_col = "qsize",
  effort_col_thresh = 3 * 3,
  out_file = NULL,
  ...
)
}
\arguments{
\item{df}{Dataframe. Cleaned data specifying context.}

\item{env_prcomp}{Output from env_pca.}

\item{context}{Character. Column names that define context, usually a 'visit'
to a 'cell'.}

\item{cores}{Numeric. Number of cores available for running chains in
rstanarm model.}

\item{response}{Character. Name to give the 'response' variable column.
Default is 'sr' for 'species richness'.}

\item{response_min_thresh}{Numeric. Threshold below which to filter \code{df}
before running the model. Default '2' excludes singleton sites.}

\item{threshold_lo, threshold_hi}{Numeric between 0 and 1 specifying the
threshold above/below which richness is excessively above or below 'normal'
and should be filtered.}

\item{effort_col}{Character (or \code{NULL}). Name of column with some measure of
effort. If \code{NULL}, all contexts will be used.}

\item{effort_col_thresh}{Numeric. \code{effort_col} (if used) will be filtered
below this threshold. This could be used to, say, filter very small quadrats.}

\item{out_file}{Character. Optional path to save output.}

\item{...}{Passed to \code{rstanarm::stan_glm()}.}
}
\value{
List of model outputs.
}
\description{
Model the effect of principal components on taxa richness.
}
