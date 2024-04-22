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

  res <- strsplit(delister(x), "")

  if (length(res) == 1) {
    return(res[[1]])
  } else {
    return(res)
  }
}
