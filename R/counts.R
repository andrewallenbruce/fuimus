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

#' Count rows and columns and pivot to wide format
#'
#' @param df A `<data.frame>` or `<tibble>`
#'
#' @param rows A `<character>` or `<symbol>` specifying the rows to count
#'
#' @param cols A `<character>` or `<symbol>` specifying the columns to count
#'
#' @examples
#' ggplot2::diamonds |>
#' count_wide(c(clarity, color), cut)
#'
#' @autoglobal
#'
#' @export
count_wide <- function(df, rows, cols) {
  df |>
    dplyr::count(
      dplyr::pick(
        c({{ rows }}, {{ cols }})
      )
    ) |>
    tidyr::pivot_wider(
      names_from = {{ cols }},
      values_from = n,
      names_sort = TRUE,
      values_fill = 0
    )
}

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
#' @returns A `<data.frame>` or `<tibble>` with the count of missing values
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

#' Show missing values
#'
#' @param df A `<data.frame>` or `<tibble>`
#'
#' @param group_vars A `<character>` or `<symbol>` vector of the variables to
#'   group by
#'
#' @param summary_vars A `<character>` or `<symbol>` vector of the variables to
#'   summarize; default is [dplyr::everything()]
#'
#' @examples
#' dplyr::tibble(x = 1:10,
#'               y = 1:10,
#'               z = c(letters[1:10])) |>
#'               show_missing(z)
#'
#' @autoglobal
#'
#' @export
show_missing <- function(df,
                         group_vars,
                         summary_vars = dplyr::everything()) {
  df |>
    dplyr::group_by(
      dplyr::pick(
        {{ group_vars }}
        )
      ) |>
    dplyr::summarize(
      dplyr::across(
        {{ summary_vars }}, \(x) sum(is.na(x)
                                     )
        ),
      .groups = "drop"
    ) |>
    dplyr::select(
      dplyr::where(\(x) any(x > 0)
                   )
      )
}
