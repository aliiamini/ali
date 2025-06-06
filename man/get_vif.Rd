% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preprocess.R
\name{get_vif}
\alias{get_vif}
\title{Check for multicollinearity using Variance Inflation Factor (VIF)}
\usage{
get_vif(df)
}
\arguments{
\item{df}{A data frame with numeric columns}
}
\value{
A named vector of VIF values
}
\description{
Calculates VIF values for each numeric predictor.
}
