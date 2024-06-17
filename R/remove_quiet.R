#' Remove empty rows and columns
#'
#' @param df data frame
#'
#' @examples
#' dplyr::tibble(
#'   x = c(1, 2, NA),
#'   y = c(NA, NA, NA)) |>
#'   remove_quiet()
#'
#' @autoglobal
#'
#' @export
remove_quiet <- function(df) {

  janitor::remove_empty(
    df,
    which = c("rows", "cols")
  )
}
