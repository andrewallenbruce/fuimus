#' Remove empty rows and columns
#'
#' @param df data frame
#'
#' @param ... additional arguments to pass to `janitor::remove_empty()`
#'
#' @examples
#' x <- dplyr::tibble(
#'   x = c(1, 2, NA),
#'   y = c(NA, NA, NA))
#'
#' x
#'
#' remove_quiet(x)
#'
#' @autoglobal
#'
#' @export
remove_quiet <- function(df, ...) {

  janitor::remove_empty(
    df,
    which = c("rows", "cols"),
    ...
  )
}
