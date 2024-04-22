#' Convert various character strings to `NA`
#'
#' @param x `<chr>` vector to convert
#'
#' @returns `<chr>` vector with converted `NA` values
#'
#' @examples
#' na_if_common(x = c(" ", "*", "--", "N/A", "", "A", "B"))
#'
#' @autoglobal
#'
#' @export
na_if_common <- function(x) {

  dplyr::case_match(x,
  c("", " ", "*", "--", "N/A") ~ NA_character_,
  .default = x)

}
