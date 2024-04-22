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
