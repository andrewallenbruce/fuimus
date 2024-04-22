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
  unlist(
    x,
    use.names = FALSE)
  }
