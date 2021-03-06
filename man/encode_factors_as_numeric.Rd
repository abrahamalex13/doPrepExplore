% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/encode_factors_as_numeric.R
\name{encode_factors_as_numeric}
\alias{encode_factors_as_numeric}
\alias{derive_posteriors_p_summaries_supervised_factor}
\alias{join_posteriors_p_summaries_supervised_factor}
\alias{transform_posteriors_p_summaries_supervised_factor}
\title{Encode factor data as a single numeric feature, via Bayesian posterior summary value.}
\usage{
derive_posteriors_p_summaries_supervised_factor(
  df,
  n_splits = 3,
  varname_factor,
  par_prior_beta,
  Y,
  directory_operators
)

join_posteriors_p_summaries_supervised_factor(
  df,
  varname_factor,
  run_type,
  directory_operators
)

transform_posteriors_p_summaries_supervised_factor(
  df,
  varname_factor,
  run_type,
  par_prior_beta,
  Y,
  directory_operators
)
}
\description{
Objective: given a factor level, derive an implied y-value.
Utilize a 'y-aware' derivation method.
Given a factor level, what does its Y vector look like?
Then consider an instance of factor level \emph{j} in rows 1:k.
To derive an implied y-value here, don't use those instances in training.
Use other available instances, in cross-validated fashion.
}
\details{
Given a factor level (and if 'train'-type execution, data split ID),
join implied-y value stored in reference operator.

Execute connected sequence for encoding factor levels
using implied y values.
}
\seealso{
\emph{Feature Engineering and Selection: A Practical Approach for Predictive Models}; Max Kuhn, Kjell Johnson.
}
