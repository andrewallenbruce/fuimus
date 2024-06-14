#' Remove empty rows and columns
#'
#' @param df data frame
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
remove_quiet <- function(df) {

  janitor::remove_empty(
    df,
    which = c("rows", "cols")
  )
}

#' `mean()` with `NA` removal
#'
#' @param x numeric vector
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
mean_na <- function(x) {

  mean(x, na.rm = TRUE)
}

#' `sum()` with `NA` removal
#'
#' @param x numeric vector
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
sum_na <- function(x) {

  sum(x, na.rm = TRUE)
}
