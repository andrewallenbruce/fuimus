% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mock_data.R
\name{is_valid_npi}
\alias{is_valid_npi}
\title{Validate NPIs}
\usage{
is_valid_npi(npi)
}
\arguments{
\item{npi}{\verb{<character>} Number of NPIs to generate}
}
\value{
\verb{<logical>} vector
}
\description{
Validate NPIs
}
\examples{
is_valid_npi(1043477615)

is_valid_npi(1234567891)

x <- random_npi_generator(100)

x[collapse::whichv(purrr::map_lgl(x, is_valid_npi), TRUE)]

}
