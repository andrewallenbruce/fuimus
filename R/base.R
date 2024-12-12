#' Create Integer Sequence Beginning at 1
#'
#' @param n `<integer>` Ending number of sequence
#'
#' @returns `<integer>` vector of numbers from 1 to `n`
#'
#' @examples
#' colon(50)
#'
#' colon(-20)
#'
#' colon(0 + 150 - 145)
#'
#' colon(20.9)
#'
#' colon(20.1)
#'
#' @autoglobal
#'
#' @keywords helpers
#'
#' @family base
#'
#' @export
colon <- function(n) {

  if (!rlang::is_integerish(n, n = length(n))) {
    rlang::warn(
      message = "`n` has been coerced to `<integer>`.",
      class = "non_int")
  }
  1:n
}

#' Linearly interpolate values between two points
#'
#' [SO Link](https://stackoverflow.com/questions/27920690/linear-interpolation-using-dplyr/31845035#31845035)
#'
#' @param x,y `<numeric>` vectors giving the coordinates of the points to be
#'   interpolated
#'
#' @param method `<character>` interpolation method, default is `"approx"`
#'
#' @returns `<numeric>` vector of interpolated values
#'
#' @examples
#' interpolate(1:5, c(10, NA, NA, NA, 100), "spline")
#'
#' df <- dplyr::tibble(
#'   seq = 1:5,
#'   v1 = c(1, NA, 3, NA, 5),
#'   v2 = c(40, NA, 60, NA, 70),
#'   v3 = c(10, NA, NA, NA, 100))
#'
#' df
#'
#' df |>
#'    dplyr::mutate(
#'    dplyr::across(
#'    .cols = dplyr::starts_with("v"),
#'    .fns = ~ interpolate(seq, .x),
#'    .names = "{.col}_est"))
#'
#' df |>
#'   dplyr::mutate(
#'    dplyr::across(
#'     dplyr::starts_with("v"),
#'      ~ interpolate(seq, .x)))
#'
#' @autoglobal
#'
#' @keywords maths
#'
#' @family base
#'
#' @export
interpolate <- function(x, y, method = c("approx", "spline")) {

  method <- match.arg(method)

  switch (
    method,
    approx = stats::approx(x, y, n = length(x))$y,
    spline = stats::spline(x, y, n = length(x))$y
  )
}

#' Wrapper for [paste0()] that collapses result
#'
#' @param x A split `<chr>` vector
#'
#' @returns A collapsed `<chr>` string
#'
#' @examples
#' collapser(c("X", "Y", "Z"))
#'
#' @autoglobal
#'
#' @export
collapser <- function(x) {
  paste0(x, collapse = "")
}

#' Wrapper for [unlist()], with `use.names` set to `FALSE`
#'
#' @param x A named `<list>`
#'
#' @returns An unnamed `<chr>` vector
#'
#' @examples
#' delister(list(x = "XYZ"))
#'
#' @autoglobal
#'
#' @export
delister <- function(x) {
  unlist(x, use.names = FALSE)
}

#' Wrapper for [strsplit()] that unlists and unnames results
#'
#' @param x `<chr>` string or named `<list>` of `<chr>` strings
#'
#' @returns An unnamed `<list>` of split `<chr>` vectors
#'
#' @examples
#' # unnamed vector
#' splitter("XYZ")
#'
#' # named vector
#' splitter(c(x = "XYZ"))
#'
#' # unnamed list with one element
#' splitter(list("XYZ"))
#'
#' # unnamed list with multiple elements
#' splitter(list("YYY", "ZZZ"))
#'
#' # named list with one element
#' splitter(list(x = "XYZ"))
#'
#' # named list with multiple elements
#' splitter(list(x = "YYY", xx = "ZZZ"))
#'
#' @autoglobal
#'
#' @export
splitter <- function(x) {

  res <- strsplit(unlist(x, use.names = FALSE), "")

  if (length(res) == 1) {
    return(res[[1]])
  } else {
    return(res)
  }
}

#' Infix `if (!is.null(x)) y else x`
#'
#' @param x,y vectors
#'
#' @examples
#' NULL %nn% 123456L
#'
#' "abc" %nn% 123456L
#'
#' @returns `y` if `x` is not `NULL`, else `x`
#'
#' @autoglobal
#'
#' @export
`%nn%` <- \(x, y) if (not_null(x)) y else x

#' Infix `!x %in% y`
#'
#' @param x,y vectors
#'
#' @examples
#' "a" %nin% c("a", "b", "c")
#'
#' @returns `TRUE` if `x` is not in `y`, else `FALSE`
#'
#' @autoglobal
#'
#' @export
`%nin%` <- \(x, y) match(x, table = y, nomatch = 0L) == 0L

#' Infix `!x %in% y`
#'
#' @param x,y vectors
#'
#' @examples
#' NULL %or% 2474701
#'
#' @returns `y` if `x` is length 0, else `x`
#'
#' @autoglobal
#'
#' @export
`%or%` <- \(x, y) if (length(x) == 0L) y else x
