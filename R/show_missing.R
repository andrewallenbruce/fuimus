#' @examplesIf interactive()
#' dplyr::tibble(x = 1:10,
#'               y = 1:10,
#'               z = letters[1:10]) |>
#'               show_missing(z)
#'
#' @autoglobal
#'
#' @noRd
show_missing <- function(df,
                         group_vars,
                         summary_vars = dplyr::everything()) {
  df |>
    dplyr::group_by(dplyr::pick({{ group_vars }})) |>
    dplyr::summarize(
      dplyr::across({{ summary_vars }}, \(x) sum(is.na(x))),
      .groups = "drop"
    ) |> dplyr::select(dplyr::where(\(x) any(x > 0)))
}
