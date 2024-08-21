#' Create Integer Sequence Beginning at 1
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
#' colon(20.9)
#'
#' colon(20.1)
#'
#' @autoglobal
#'
#' @export
colon <- function(n) {

  if (!rlang::is_integerish(n, n = length(n))) {
    rlang::warn(
      message = "`n` has been coerced to an integer.",
      class = "non_int")
  }
  1:n
}
