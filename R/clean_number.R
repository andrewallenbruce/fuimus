#' Clean character vector of numbers
#'
#' @param x `<chr>` vector of numbers
#'
#' @returns a `<dbl>` vector of numbers
#'
#' @examples
#' clean_number(c("20%", "21,125,458", "$123"))
#'
#' @autoglobal
#'
#' @export
clean_number <- function(x) {

  is_pct <- stringr::str_detect(x, "%")

  x <- x |>
    stringr::str_remove_all("%") |>
    stringr::str_remove_all(",") |>
    stringr::str_remove_all(stringr::fixed("$")) |>
    as.numeric(x)

  dplyr::if_else(is_pct, x / 100, x)
}
