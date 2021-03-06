% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stats.R
\name{get_corr_table}
\alias{get_corr_table}
\title{Correlation table}
\usage{
get_corr_table(data, vars)
}
\arguments{
\item{data}{Input data}

\item{vars}{named character vector. The variable names and labels of
variables to compute correlations for}
}
\value{
tbl. Correlation table
}
\description{
Computes correlations between given variables. Returns correlation table as a
data frame.
}
