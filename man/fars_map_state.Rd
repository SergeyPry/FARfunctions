% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FARfunctions.R
\name{fars_map_state}
\alias{fars_map_state}
\title{The function \code{fars_map_state} takes the value of state number and year to produce a map with
accidents marked}
\usage{
fars_map_state(state.num, year)
}
\arguments{
\item{state.num}{numerical ID of a state}

\item{year}{integer value of the year}
}
\description{
The function \code{fars_map_state} takes the value of state number and year to produce a map with
accidents marked
}
\note{
This function does not have any data ourput, instead, it produces a plot as a side effect of the function
}
\examples{
 \dontrun{fars_map_state(10, 2015)}

}
