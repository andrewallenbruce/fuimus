% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/string_ops.R
\name{splitter}
\alias{splitter}
\title{Wrapper for \code{\link[=strsplit]{strsplit()}} that unlists and unnames results}
\usage{
splitter(x)
}
\arguments{
\item{x}{\verb{<chr>} string or named \verb{<list>} of \verb{<chr>} strings}
}
\value{
An unnamed \verb{<list>} of split \verb{<chr>} vectors
}
\description{
Wrapper for \code{\link[=strsplit]{strsplit()}} that unlists and unnames results
}
\examples{
# unnamed vector
splitter("XYZ")

# named vector
splitter(c(x = "XYZ"))

# unnamed list with one element
splitter(list("XYZ"))

# unnamed list with multiple elements
splitter(list("YYY", "ZZZ"))

# named list with one element
splitter(list(x = "XYZ"))

# named list with multiple elements
splitter(list(x = "YYY", xx = "ZZZ"))

}
