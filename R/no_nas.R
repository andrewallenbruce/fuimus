#' `mean()` with `NA` removal
#'
#' @param x numeric vector
#'
#' @examples
#' mean_na(c(1, 2, NA))
#'
#' @autoglobal
#'
#' @export
mean_na <- function(x) {

  mean(x, na.rm = TRUE)
}

#' `sum()` with `NA` removal
#'
#' @param x numeric vector
#'
#' @examples
#' sum_na(c(1, 2, NA))
#'
#' @autoglobal
#'
#' @export
sum_na <- function(x) {

  sum(x, na.rm = TRUE)
}
