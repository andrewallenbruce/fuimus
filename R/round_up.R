#' Round a numeric vector up to the nearest decimal place
#'
#' Wrapper around [janitor::round_half_up()]
#'
#' @param x A numeric vector
#'
#' @param digits `<int>` Number of decimal places to round to
#'
#' @autoglobal
#'
#' @export
roundup <- function(x, digits = 2) {
  janitor::round_half_up(x, digits = digits)
}
