% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidyverse.R
\name{gchop}
\alias{gchop}
\title{Chop Vector by Group}
\usage{
gchop(v, g)
}
\arguments{
\item{v}{\verb{<character>} vector}

\item{g}{\verb{<integer>} group}
}
\value{
\verb{<list>}
}
\description{
Chop Vector by Group
}
\examples{
(v <- c("222", "280", "3020", "8690", "G0294", "G8126"))

(g <- sample(1:2, size = length(v), replace = TRUE))

gchop(v, g)

}
\seealso{
Other tidyverse: 
\code{\link{columns_to_character}()},
\code{\link{glue_chr}()},
\code{\link{glue_data_chr}()},
\code{\link{named_group_split}()},
\code{\link{search_for}()}
}
\concept{tidyverse}
\keyword{helpers}
