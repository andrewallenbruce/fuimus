#' Generate simple data frame for testing
#'
#' @param years `<int>` vector (`c(2020, 2021, 2023)`) or sequence of years
#'   (`2010:2020`)
#'
#' @returns A [tibble][tibble::tibble-package] containing the search results.
#'
#' @examples
#' mock_provider(2010:2020)
#'
#' mock_provider(c(2020, 2021, 2023))
#'
#' @autoglobal
#'
#' @family mock
#'
#' @export
mock_provider <- \(years) {

  lng <- length(years)

  dplyr::tibble(
    year = rep.int(years, 2),
    group = rep(c("A", "B"), each = lng),
    payment = sample(10000:20000, lng * 2))
}

#' Generate mock coding/billing data frame
#'
#' @param rows `<int>` number of rows to generate; default is 10
#'
#' @param nest `<lgl>` whether to nest the dates column; default is `FALSE`
#'
#' @returns A [tibble][tibble::tibble-package]
#'
#' @examples
#' mock_forager(rows = 5)
#'
#' @autoglobal
#'
#' @family mock
#'
#' @export
mock_forager <- function(rows = 10, nest = FALSE){

  payers <- sample(
    x = factor(
      x = c("Medicare",
            "Medicaid",
            "Cigna",
            "Humana",
            "UHC",
            "Anthem",
            "BCBS",
            "Centene",
            "MAO")),
    size = rows,
    replace = TRUE)

  classes <- sample(
    x = ordered(
      c("Primary",
        "Secondary")),
    size = rows,
    replace = TRUE)

  x <- dplyr::tibble(
    id                   = sprintf(paste0("%0", nchar(rows) + 3, "d"), seq_len(rows)),
    payer                = payers,
    class                = classes,
    balance              = roundup(stats::rgamma(n = rows, 2) * 20000) / 300,
    date_of_service      = sample(x = seq.Date(from = Sys.Date(), by = "-1 months", length.out = 12), size = rows, replace = TRUE),
    date_of_release      = date_of_service + roundup(abs(stats::rnorm(rows, 11, 4))),
    date_of_submission   = date_of_release + roundup(abs(stats::rnorm(rows, 2, 2))),
    date_of_acceptance   = date_of_submission + roundup(abs(stats::rnorm(rows, 3, 2))),
    date_of_adjudication = date_of_acceptance + roundup(abs(stats::rnorm(rows, 30, 3)))
    )

  if (nest)
    return(
      tidyr::nest(
        x,
        dates = tidyr::contains("date")
        )
      )
  x
}
