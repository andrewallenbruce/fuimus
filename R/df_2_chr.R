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
