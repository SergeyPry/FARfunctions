% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FARfunctions.R
\name{fars_summarize_years}
\alias{fars_summarize_years}
\title{The function \code{fars_summarize_years} takes a list of data frames produced by the function \code{fars_read_years}
and produces a summary table of the number of observations attributed to each month over the years, for which data are
available.}
\usage{
fars_summarize_years(years)
}
\arguments{
\item{years}{a vector of integer year values}
}
\value{
A data frame containing months as observations, years as column names and number of individual
observations as values
}
\description{
The function \code{fars_summarize_years} takes a list of data frames produced by the function \code{fars_read_years}
and produces a summary table of the number of observations attributed to each month over the years, for which data are
available.
}
\examples{
\dontrun{fars_summarize_years(c(2014, 2015))}

}
