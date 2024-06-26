% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dates.R
\name{make_interval}
\alias{make_interval}
\title{Create interval, period and time length columns from a start and end date}
\usage{
make_interval(df, start, end = lubridate::today())
}
\arguments{
\item{df}{A \verb{<data.frame>} or \verb{<tibble>}}

\item{start}{\verb{<date>} Start date column}

\item{end}{\verb{<date>} End date column}
}
\value{
\verb{<data.frame>} or \verb{<tibble>}
}
\description{
Create interval, period and time length columns from a start and end date
}
\examples{
dplyr::tibble(date = lubridate::today() - 366) |>
  make_interval(start = date)

}
