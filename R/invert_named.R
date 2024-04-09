#' Invert a named vector
#'
#' @param x A named vector
#'
#' @returns A named vector with names and values inverted
#'
#' @examples
#' invert_named(x = c(name = "element"))
#'
#' invert_named(x = c(element = "name"))
#'
#' @autoglobal
#'
#' @export
invert_named <- function(x) {

  stopifnot(
    "Input must be a named vector" = !is.null(names(x)))

  rlang::set_names(names(x), unname(x))
}
