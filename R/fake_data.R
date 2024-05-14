#' Generate Data for Testing
#'
#' @param year_seq sequence of years, e.g. `2010:2020`
#'
#' @returns A [tibble][tibble::tibble-package] containing the search results.
#'
#' @examples
#' provider_data(2010:2020)
#'
#'
#' @autoglobal
#'
#' @export
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
