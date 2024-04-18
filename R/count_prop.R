#' Count and calculate proportion of each group
#'
#' @param df A `<data.frame>` or `<tibble>`
#'
#' @param var A `<character>` or `<symbol>` specifying the column to count
#'
#' @param sort A `<logical>` indicating whether to sort the output by frequency,
#'   default is `FALSE`
#'
#' @param na.rm A `<logical>` indicating whether to remove missing values from
#'   the count, default is `FALSE`
#'
#' @examples
#' dplyr::tibble(
#'    x = rnorm(5),
#'    y = rnorm(5),
#'    z = rnorm(5)
#'    ) |>
#'    count_prop(x)
#'
#' @autoglobal
#'
#' @export
count_prop <- function(df,
                       var,
                       sort = FALSE,
                       na.rm = FALSE) {
  df |>
    dplyr::count({{ var }}, sort = sort) |>
    dplyr::mutate(prop = n / sum(n, na.rm = na.rm))
}

#' Count and calculate proportion of each group
#'
#' @param df A `<data.frame>` or `<tibble>`
#'
#' @param rows A `<character>` or `<symbol>` specifying the rows to count
#'
#' @param cols A `<character>` or `<symbol>` specifying the columns to count
#'
#' @param sort A `<logical>` indicating whether to sort the output by frequency,
#'   default is `FALSE`
#'
#' @param na.rm A `<logical>` indicating whether to remove missing values from
#'   the count, default is `FALSE`
#'
#' @examples
#' ggplot2::diamonds |> head()
#'
#' ggplot2::diamonds |>
#'   count_prop_multi(
#'   c(clarity, color),
#'   cut,
#'   sort = TRUE)
#'
#' @autoglobal
#'
#' @export
count_prop_multi <- function(df,
                             rows,
                             cols,
                             sort = FALSE,
                             na.rm = FALSE) {
  df |>
    dplyr::count(
      dplyr::pick(
        c(
          {{ rows }},
          {{ cols }}
          )
        ),
      sort = sort
      ) |>
    dplyr::mutate(prop = n / sum(n, na.rm = na.rm))
}
