#' Count and calculate proportion of each group
#'
#' @param df A `<data.frame>` or `<tibble>`
#'
#' @param var A `<character>` or `<symbol>` specifying the column to count
#'
#' @param sort A `<logical>` indicating whether to sort the output by frequency,
#'   default is `FALSE`
#'
#' @examples
#' dplyr::tibble(x = 1:10,
#'               y = sample(1:1000, length(x))) |>
#'               count_prop(x)
#'
#' @autoglobal
#'
#' @export
count_prop <- function(df, var, sort = FALSE) {
  df |>
    dplyr::count({{ var }}, sort = sort) |>
    dplyr::mutate(prop = n / sum(n))
}
