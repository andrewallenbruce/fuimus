% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/maths.R
\name{rate_of_return}
\alias{rate_of_return}
\title{Lagged rate of return}
\usage{
rate_of_return(df, col, n = 1, fill_na = 1L)
}
\arguments{
\item{df}{data frame}

\item{col}{numeric column}

\item{n}{\verb{<int>} values to offset}

\item{fill_na}{\verb{<int>} fill value for any NAs; default is 1}
}
\description{
Lagged rate of return
}
\examples{
fuimus:::provider_data(2020:2025) |>
  dplyr::group_by(group) |>
  rate_of_return(net_payment)

}
