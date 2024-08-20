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
