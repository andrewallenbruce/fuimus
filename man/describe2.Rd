% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/describe.R
\name{describe2}
\alias{describe2}
\title{Describe 2}
\usage{
describe2(df, ...)
}
\arguments{
\item{df}{\verb{<data.frame>} desc}

\item{...}{\verb{<dots>} tidyselect columns}
}
\value{
\verb{<tibble>} of summary statistics
}
\description{
Describe 2
}
\examples{
describe2(mock_provider(2000:2020))

describe2(mock_forager(200))

}
