#' Return NULL if the input is empty, otherwise return the input
#'
#' @param x vector
#'
#' @returns vector or `NULL`
#'
#' @examples
#' null_if_empty(c())
#'
#' null_if_empty(c(1))
#'
#' @autoglobal
#'
#' @export
null_if_empty <- function(x) {
  if (vctrs::vec_is_empty(x)) NULL else x
}
