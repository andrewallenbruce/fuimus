#' Coerce numeric columns to character
#'
#' @param df `<data.frame>` or `<tibble>`
#'
#' @returns A `<data.frame>` or `<tibble>` with numeric columns coerced to character
#'
#' @examples
#' dplyr::tibble(
#'    int = 1:10,
#'    chr = letters[1:10],
#'    date = rep(Sys.Date()), nrow(int)) |>
#' df_2_chr()
#'
#' @autoglobal
#'
#' @export
df_2_chr <- function(df) {
  df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(), as.character))
}

#' Get the types of each column in a data frame
#'
#' @param df A data frame
#'
#' @examples
#' dplyr::tibble(
#'    int = 1:10,
#'    chr = letters[1:10],
#'    date = rep(Sys.Date()), nrow(int)) |>
#' df_types()
#'
#' @export
#'
#' @autoglobal
df_types <- function(df) {
  dplyr::tibble(
    col_name = names(df),
    col_type = purrr::map_chr(df, vctrs::vec_ptype_full),
    n_miss = purrr::map_int(df, \(x) sum(is.na(x)))
  )
}

#' Pivot data frame to long format for easy printing
#'
#' @param df `<data.frame>` or `<tibble>` to pivot long
#'
#' @param cols `<character>` vector of columns to pivot long, default is [dplyr::everything()]
#'
#' @returns a `<data.frame>` or `<tibble>` in long format
#'
#' @examples
#' dplyr::tibble(
#'    int = 1:10,
#'    chr = letters[1:10],
#'    date = rep(Sys.Date()), nrow(int)) |>
#' display_long()
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

#' Combine multiple columns into one
#'
#' @param df `<data.frame>` or `<tibble>`
#'
#' @param name new column name, unquoted, default is `combined`
#'
#' @param columns `<chr>` vector of columns to combine
#'
#' @param sep separator between combined row data, default is `"-"`
#'
#' @returns A `<data.frame>` or `<tibble>` with combined columns
#'
#' @examples
#' x <- fuimus:::forager_data()[-5]
#'
#' x[1, 2] <- ""
#'
#' x
#'
#' x |>
#'   combine(
#'     name = id_payer,
#'     columns = c('claim_id', 'payer'))
#'
#' @autoglobal
#'
#' @export
combine <- function(df, name = combined, columns, sep = "-") {

  x <- tidyr::unite(
    df,
    col = {{ name }},
    dplyr::any_of(columns),
    remove = TRUE,
    na.rm = TRUE,
    sep = sep)

  x |>
    dplyr::mutate(
      {{ name }} := dplyr::na_if({{ name }}, "")
    )
}

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
#' fuimus:::forager_data() |>
#'   count_prop(payer, sort = TRUE)
#'
#' @autoglobal
#'
#' @export
count_prop <- function(df,
                       var,
                       sort = FALSE,
                       na.rm = FALSE) {
  df |>
    dplyr::count(
      {{ var }},
      sort = sort
    ) |>
    dplyr::mutate(
      prop = n / sum(n, na.rm = na.rm)
    )
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
#' fuimus:::forager_data(10) |>
#'   count_prop_multi(
#'   c(ins_class),
#'   payer,
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
        )),
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
#' fuimus:::forager_data(10) |>
#'   count_wide(c(ins_class), payer)
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
      names_from  = {{ cols }},
      values_from = n,
      names_sort  = TRUE,
      values_fill = 0
    )
}
