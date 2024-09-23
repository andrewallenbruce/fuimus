#' Calculate the percentage change between two values
#'
#' Percentage change is obtained by dividing the change in value by the original
#' value.
#'
#' If the result is positive, then it is an increase. If the result is negative,
#' then it is a decrease.
#'
#' @param new `<dbl>` newer value
#'
#' @param old `<dbl>` older value
#'
#' @returns `<dbl>` vector of percentage change
#'
#' @examples
#' percentage_change(new = 949740, old = 942482)
#'
#' percentage_change(new = 942482, old = 1132783)
#'
#' @autoglobal
#'
#' @export
percentage_change <- function(new, old) {

  stopifnot(
    is.numeric(new),
    is.numeric(old)
  )

  (new - old) / old
}

#' Calculate the new value from the old value and the percentage change between
#' the two values
#'
#' @param percentage_change `<dbl>` percentage change
#'
#' @param old `<dbl>` older value
#'
#' @returns `<dbl>` vector of new value
#'
#' @examples
#' new_value(old = 942482, percentage_change = 0.007700943)
#'
#' new_value(old = 1132783, percentage_change = -0.1679942)
#'
#' # If the value of an object increased by 220%,
#' # from the original value of
#' # $500,000, what is it worth now?
#'
#' new_value(old = 500000, percentage_change = 2.2)
#'
#' @autoglobal
#'
#' @export
new_value <- function(old, percentage_change) {

  stopifnot(
    is.numeric(percentage_change),
    is.numeric(old)
  )

  old + (percentage_change * old)
}

#' Calculate the percentage difference between two values
#'
#' Percentage difference is obtained by dividing the absolute value of change by
#' the average of the values and then multiplying it with 100.
#'
#' Since the absolute value is taken for the change (or difference) in values,
#' the order of the numbers does not matter.
#'
#' @param x,y `<dbl>` values to determine the percent difference between, order
#'   does not matter
#'
#' @returns `<dbl>` vector of percentage difference
#'
#' @examples
#' percentage_difference(949740, 942482)
#'
#' percentage_difference(942482, 1132783)
#'
#' @autoglobal
#'
#' @export
percentage_difference <- function(x, y) {

  stopifnot(
    is.numeric(x),
    is.numeric(y)
  )

  abs(x - y) / mean(c(x, y))
}

#' Percentage calculator
#'
#' If _a_ is some _p%_ of _b_, then:
#'    + _p_ = a/b
#'    + _a_ = p * b
#'    + _b_ = a/p
#'
#' Since the absolute value is taken for the change (or difference) in values,
#' the order of the numbers does not matter.
#'
#' @param a,b `<dbl>` values
#'
#' @returns `<dbl>` vector of percentage difference
#'
#' @examples
#' percentage_calculator(2500, 133)
#'
#' percentage_calculator(133, 2500)
#'
#' @autoglobal
#'
#' @export
percentage_calculator <- function(a, b) {

  stopifnot(
    is.numeric(a),
    is.numeric(b)
  )

  list(
    p = a / b,
    a = b * (a / b),
    b = a / (a / b)
  )
}
