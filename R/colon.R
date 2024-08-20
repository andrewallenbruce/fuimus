#' Number Sequence Starting at 1
#'
#' @param n `<integer>` Ending number of sequence
#'
#' @returns `<integer>` vector of numbers from 1 to `n`
#'
#' @examples
#' colon(50)
#'
#' colon(-20)
#'
#' colon(0 + 150 - 145)
#'
#' @autoglobal
#'
#' @export
colon <- function(n) {
  1:n
}
