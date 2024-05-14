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
