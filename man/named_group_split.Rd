% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidyverse.R
\name{named_group_split}
\alias{named_group_split}
\title{Split a \verb{<tibble>} by Groups with Named List Output}
\usage{
named_group_split(df, ...)
}
\arguments{
\item{df}{\verb{<tibble>} or \verb{<data.frame>} to split}

\item{...}{One or more unquoted variables by which to group and then split
\code{df}. Variables can be separated by commas.}
}
\value{
named \verb{<list>} of tibbles, the names of which are derived from the
unique combinations of grouping variables, separated by "_".
}
\description{
This function takes a \verb{<tibble>}, groups it by one or more variables, and
splits the grouped data into a list. The resulting list has names derived
from the unique combinations of the grouping variables.
}
\examples{
(x <- dplyr::tibble(
  zip = c("Data_Weekly.zip",
        "Data_Weekly.zip",
        "Data_April.zip",
        "Deactivated.zip"),
  file = c(
    "npidata.csv",
    "npidata2.csv",
    "endpoint.csv",
    "Deactivated.xlsx")))

named_group_split(x, zip)

}
\seealso{
Other tidyverse: 
\code{\link{columns_to_character}()},
\code{\link{gchop}()},
\code{\link{glue_chr}()},
\code{\link{glue_data_chr}()},
\code{\link{search_for}()}
}
\concept{tidyverse}
\keyword{helpers}
