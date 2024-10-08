% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/string_ops.R
\name{rename_seq}
\alias{rename_seq}
\title{Generate a sequence of numbers with a new prefix}
\usage{
rename_seq(
  n,
  new,
  between = " = ",
  old,
  collapse = ", ",
  enclose = NULL,
  style = TRUE
)
}
\arguments{
\item{n}{\verb{<int>} Numeric sequence to generate}

\item{new}{\verb{<chr>} New prefix}

\item{between}{\verb{<chr>} Separator between \code{new} and \code{old}, default \code{" = "}}

\item{old}{\verb{<chr>} Old prefix}

\item{collapse}{\verb{<chr>} Separator between sequences, default \code{", "}}

\item{enclose}{\verb{<chr>} \emph{(optional)} Vector of \code{length(x) == 2} with which to enclose output}

\item{style}{\verb{<lgl>} Apply \code{styler::style_text()} to output, default \code{TRUE}}
}
\value{
\verb{<chr>} collapsed vector of \code{n} sequences
}
\description{
Generate a sequence of numbers with a new prefix
}
\examples{
rename_seq(
   n        = 10,
   new      = "id_issuer_",
   between  = " = ",
   old      = "Other.ID.Issuer.",
   enclose  = c("x = c(", ")"),
   collapse = ",\n ",
   style    = TRUE)

}
