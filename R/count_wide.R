#' @examplesIf interactive()
#' ggplot2::diamonds |>
#' count_wide(c(clarity, color), cut)
#'
#' @autoglobal
#'
#' @noRd
count_wide <- function(data, rows, cols) {
  data |>
    dplyr::count(dplyr::pick(c({{ rows }}, {{ cols }}))) |>
    tidyr::pivot_wider(
      names_from = {{ cols }},
      values_from = n,
      names_sort = TRUE,
      values_fill = 0
    )
}
