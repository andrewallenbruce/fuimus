#' Return GitHub raw url
#'
#' @param x `<chr>` string
#'
#' @returns `<chr>` GitHub raw url
#'
#' @examples
#' gh_raw("andrewallenbruce/example/main/inst/pins/")
#'
#' @autoglobal
#'
#' @export
gh_raw <- function(x) {
  paste0(
    "https://raw.githubusercontent.com/",
    x
    )
}
