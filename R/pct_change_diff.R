#' Calculate the percentage change between two values
#'
#' Percentage change is obtained by dividing the change in value by the original
#' value. If the result is positive, then it is an increase. If the result is
#' negative, then it is a decrease.
#'
#' @param new `<dbl>` newer value
#'
#' @param old `<dbl>` older value
#'
#' @returns vector
#'
#' @examples
#' pct_change(new = 1, old = 100)
#'
#' pct_change(new = 100, old = 1)
#'
#' @autoglobal
#'
#' @export
pct_change <- function(new, old) {
  (new - old) / old
  }

#' Calculate the percent difference between two values
#'
#' Percent difference is obtained by dividing the absolute value of change by
#' the average of the values and then multiplying it with 100. Since the
#' absolute value is taken for the change (or difference) in values, the order
#' of the numbers does not matter.
#'
#' @param x,y `<dbl>` values to determine the percent difference between.
#'
#' @returns vector
#'
#' @examples
#' pct_difference(1, 100)
#'
#' pct_difference(100, 1)
#'
#' @autoglobal
#'
#' @export
pct_difference <- function(x, y) {
  abs(x - y) / mean(
    c(x, y)
    )
  }
