#' @examplesIf interactive()
#' dplyr::tibble(x = 1:10, y = 1:10) |> df_types()
#'
#' @autoglobal
#'
#' @noRd
df_types <- function(df) {
  dplyr::tibble(
    col_name = names(df),
    col_type = purrr::map_chr(df, vctrs::vec_ptype_full),
    n_miss = purrr::map_int(df, \(x) sum(is.na(x)))
  )
}
