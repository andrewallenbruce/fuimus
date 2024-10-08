% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/df.R
\name{df_types}
\alias{df_types}
\title{Get the types of each column in a data frame}
\usage{
df_types(df)
}
\arguments{
\item{df}{A data frame}
}
\description{
Get the types of each column in a data frame
}
\examples{
dplyr::tibble(
   int = 1:10,
   chr = letters[1:10],
   date = rep(Sys.Date()), nrow(int)) |>
df_types()

}
