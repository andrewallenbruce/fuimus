#' Convert various character values to `NA`
#'
#' @param x `<chr>` vector
#'
#' @returns `<chr>` vector with `NA` values
#'
#' @examples
#' na_blank(x = c(" ", "*", "--", "N/A", ""))
#'
#' @autoglobal
#'
#' @export
na_blank <- function(x) {

  y <- c("", " ", "*", "--", "N/A")

  x <- dplyr::na_if(x, y)

}
