% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/maths.R
\name{change}
\alias{change}
\title{Calculate Lagged Metrics}
\usage{
change(df, cols, csm = NULL)
}
\arguments{
\item{df}{data frame}

\item{cols}{numeric columns to calculate absolute/relative change & rate of return}

\item{csm}{numeric cols to calculate cumulative sum for}
}
\description{
Calculate Lagged Metrics
}
\examples{
fuimus:::provider_data(2020:2025) |>
  dplyr::group_by(group) |>
  change(net_payment, csm = c("payment", "_chg"))

}
