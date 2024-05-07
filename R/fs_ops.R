#' Test if a path is a directory
#'
#' @param x `<chr>` directory path to check
#'
#' @returns named `<lgl>` vector, where the names give the paths. If the given
#'    object does not exist, `NA` is returned.
#'
#' @examples
#' is_directory("C:/")
#'
#' @autoglobal
#'
#' @export
is_directory <- function(x) {
  fs::is_dir(x)
}

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
