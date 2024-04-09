#' Test if a path is readable
#'
#' @param x `<chr>` vector of one or more paths
#'
#' @returns `<lgl>` vector, with names corresponding to input path.
#'
#' @examples
#' is_readable("D:/")
#'
#' @autoglobal
#'
#' @export
is_readable <- function(x) {
  fs::file_exists(x)
  }
