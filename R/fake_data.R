#' Generate Data for Testing
#'
#' @param year_seq sequence of years, e.g. `2010:2020`
#'
#' @returns A [tibble][tibble::tibble-package] containing the search results.
#'
#' @examplesIf interactive()
#' provider_data(2010:2020)
#'
#' @autoglobal
#'
#' @noRd
provider_data <- function(year_seq) {

  lng <- length(year_seq) * 2

  vctrs::vec_rbind(
    dplyr::tibble(
      year = {{ year_seq }},
      group = "A"),
    dplyr::tibble(
      year = {{ year_seq }},
      group = "B")
    ) |>
    dplyr::mutate(
      net_payment = sample(
        10000:20000, lng
        )
      )
}

#' Generate mock coding/billing data frame
#'
#' @param rows number of rows to generate; default is 10
#'
#' @param unnest a logical indicating whether to unnest the dates column; default is `FALSE`
#'
#' @returns A [tibble][tibble::tibble-package]
#'
#' @examplesIf interactive()
#' forager_data(rows = 5)
#'
#' @autoglobal
#'
#' @noRd
forager_data <- function(rows = 10, unnest = FALSE){

  x <- dplyr::tibble(
    claim_id        = as.character(wakefield::id(n = rows)),
    date_of_service = wakefield::date_stamp(n = rows,
      start         = lubridate::today() - lubridate::dyears(2),
      random        = TRUE),
    payer           = fixtuRes::set_vector(rows,
      set           = c("Medicare", "Medicaid", "Cigna", "Humana", "UnitedHealth", "Anthem", "BCBS", "Centene")),
    ins_class       = fixtuRes::set_vector(rows, set = c("Primary", "Secondary")),
    balance         = as.double(wakefield::income(n = rows, digits = 2) / 300)) |>
    dplyr::mutate(
      date_of_service      = lubridate::as_date(date_of_service),
      date_of_release      = date_of_service + round(abs(stats::rnorm(length(date_of_service), 11, 4))),
      date_of_submission   = date_of_release + round(abs(stats::rnorm(length(date_of_release), 2, 2))),
      date_of_acceptance   = date_of_submission + round(abs(stats::rnorm(length(date_of_submission), 3, 2))),
      date_of_adjudication = date_of_acceptance + round(abs(stats::rnorm(length(date_of_acceptance), 30, 3)))) |>
    tidyr::nest(dates = tidyr::contains("date"))

  if(unnest) x <- tidyr::unnest_wider(x, dates)

  return(x)
}
