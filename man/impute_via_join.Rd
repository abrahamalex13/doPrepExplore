% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/impute_via_join.R
\name{impute_via_join}
\alias{impute_via_join}
\title{Impute NA values via join with reference data.}
\usage{
impute_via_join(x, imputation_reference, by)
}
\arguments{
\item{x}{A data frame.}

\item{imputation_reference}{A data frame.}

\item{by}{Character vector of by-group variable names.}
}
\value{
Modified form of data frame \code{x}, with NA values imputed where possible.
}
\description{
Impute NA values via join with reference data.
}
