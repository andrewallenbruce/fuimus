% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/experiments.R
\name{find_common_order}
\alias{find_common_order}
\title{Determine a common ordering from a set of vectors}
\usage{
find_common_order(..., .strict = TRUE)
}
\arguments{
\item{...}{Input vectors}

\item{.strict}{If \code{TRUE}, an error will be thrown if no common ordering
exists. If \code{FALSE}, a warning will be thrown and the output will be
returned. Note that to use the output in such cases, you may want to
call \code{unique()} to remove any duplicates.}
}
\value{
\verb{<character>} vector of \code{n} NPIs
}
\description{
Determine a common ordering from a set of vectors
}
\examples{
\dontshow{if (FALSE) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}

find_common_order(
  c(1, 3, 4, 7, 8),
  c(2, 3, 4, 8, 9),
  c(4, 5, 6, 7, 9)
)

find_common_order(
  c("bananas", "oranges", "apples"),
  c("oranges", "avocados", "apples"),
  c("apples", "mangos", "lemons")
)

find_common_order(
  c("foo", "bar"),
  c("bar", "baz"),
  c("baz", "foo")
)
\dontshow{\}) # examplesIf}
}
