#' Coerce numeric columns to character
#'
#' @param df `<data.frame>` or `<tibble>`
#'
#' @returns A `<data.frame>` or `<tibble>` with numeric columns coerced to character
#'
#' @examples
#' df_2_chr(dplyr::tibble(x = 20:35))
#'
#' @autoglobal
#'
#' @export
df_2_chr <- function(df) {
  df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::where(is.numeric), as.character))
}

#' Get the types of each column in a data frame
#'
#' @param df A data frame
#'
#' @examples
#' dplyr::tibble(x = 1:10, y = 1:10) |> df_types()
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
