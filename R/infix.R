#' Infix operator for `if (!is.null(x)) y else x` statements
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
`%nn%` <- function(x, y) {
  if (!is.null(x))
    y
  else
    x
}

#' Infix operator for `not in` statements
#'
#' @param x vector
#'
#' @param table vector
#'
#' @returns logical vector
#'
#' @autoglobal
#'
#' @export
`%nin%` <- function(x, table) {
  match(x, table, nomatch = 0L) == 0L
}
