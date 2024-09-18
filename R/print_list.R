#' Print a named list
#'
#' @param ls `<list>` to print
#'
#' @param prefix `<chr>` to prepend to each line
#'
#' @returns `<list>` invisibly
#'
#' @examples
#' print_ls(list(a = 1, b = 2, c = 3))
#'
#' @autoglobal
#'
#' @export
print_ls <- function(ls, prefix = "") {

  if (length(ls) == 0) cat("<empty>\n")

  ns <- names(ls)

  if (length(ns) != length(ls)) stop("all elements must be named")

  ls <- lapply(ls, as.character)

  cat(sprintf("%s%s : %s", prefix, format(ns), ls), sep = "\n")

  invisible(ls)
}
