% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spread_factor_features.R
\name{spread_factor_features}
\alias{spread_factor_features}
\title{Spread each factor-type variable to binary indicator columns.}
\usage{
spread_factor_features(df, omit_one_level = FALSE)
}
\arguments{
\item{df}{A data frame.}

\item{omit_one_level}{One factor level is not spread (avoids perfect collinearity)}
}
\value{
A data frame.
}
\description{
One factor level maps to one binary indicator.
}
