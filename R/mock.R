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
#' @param nest `<lgl>` whether to nest the dates column; default is `TRUE`
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
mock_forager <- function(rows = 10, nest = TRUE){

  payer_names <- c("Medicare", "Medicaid", "Cigna", "Humana", "UHC", "Anthem", "BCBS", "Centene")

  x <- dplyr::tibble(
    claim_id             = as.character(wakefield::id(n = rows)),
    date_of_service      = wakefield::dob(n = rows, start = Sys.Date() - 730, random = TRUE, k = 12, by = "-1 months"),
    payer                = fixtuRes::set_vector(rows, set = payer_names),
    ins_class            = fixtuRes::set_vector(rows, set = c("Primary", "Secondary")),
    balance              = as.double(wakefield::income(n = rows, digits = 2) / 300),
    date_of_release      = date_of_service + round(abs(stats::rnorm(length(date_of_service), 11, 4))),
    date_of_submission   = date_of_release + round(abs(stats::rnorm(length(date_of_release), 2, 2))),
    date_of_acceptance   = date_of_submission + round(abs(stats::rnorm(length(date_of_submission), 3, 2))),
    date_of_adjudication = date_of_acceptance + round(abs(stats::rnorm(length(date_of_acceptance), 30, 3))))

  if (nest)
    return(tidyr::nest(x, dates = tidyr::contains("date")))

  x
}
