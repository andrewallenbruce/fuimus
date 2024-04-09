#' Pivot data frame to long format for easy printing
#'
#' @param df `<data.frame>` or `<tibble>` to pivot long
#'
#' @param cols `<character>` vector of columns to pivot long, default is [dplyr::everything()]
#'
#' @returns a `<data.frame>` or `<tibble>` in long format
#'
#' @examples
#' x <- dplyr::tibble(
#'      a = 1:10,
#'      b = letters[1:10],
#'      c = 11:20,
#'      d = LETTERS[1:10],
#'      e = 21:30)
#'
#' display_long(x)
#'
#' @autoglobal
#'
#' @export
display_long <- function(df, cols = dplyr::everything()) {

  df |> dplyr::mutate(
    dplyr::across(
      dplyr::everything(), as.character)) |>
    tidyr::pivot_longer({{ cols }})
}
