#' Count missing values
#'
#' @param df A `<data.frame>` or `<tibble>`
#'
#' @param group_vars A `<character>` or `<symbol>` vector of the variables to
#'   group by
#'
#' @param x_var A `<character>` or `<symbol>`  vector of the variable to count
#'   missing values for
#'
#' @examples
#' dplyr::tibble(x = 1:10,
#'               y = 1:10,
#'               z = letters[1:10]) |>
#'               count_missing(z, x)
#' @autoglobal
#'
#' @export
count_missing <- function(df,
                          group_vars,
                          x_var) {
  df |>
    dplyr::group_by(dplyr::pick({{ group_vars }})) |>
    dplyr::summarize(
      n_miss = sum(is.na({{ x_var }})),
      .groups = "drop"
    )
}
