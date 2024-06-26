% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dates.R
\name{duration_vec}
\alias{duration_vec}
\title{Calculate the duration between two dates}
\usage{
duration_vec(from, to)
}
\arguments{
\item{from}{\verb{<date>} Start date}

\item{to}{\verb{<date>} End date}
}
\value{
\verb{<dbl>} vector of the duration between the two dates
}
\description{
Calculate the duration between two dates
}
\examples{
duration_vec(
  from = lubridate::today() - 366,
  to = lubridate::today())

}
