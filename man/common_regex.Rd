% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/regex.R
\name{common_regex}
\alias{common_regex}
\title{Common Regular expressions}
\usage{
common_regex(x = c("month_date", "month", "url"))
}
\arguments{
\item{x}{\verb{<chr>} regex name}
}
\value{
\verb{<chr>} string of a regex
}
\description{
Common Regular expressions
}
\examples{
common_regex("url")

common_regex("month")

common_regex("month_date")

}
