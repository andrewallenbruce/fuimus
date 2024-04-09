#' Search a data frame column by string
#'
#' @param df `<data.frame>` or `<tibble>`
#'
#' @param col column name as string or unquoted
#'
#' @param search string to search for, can be a regular expression, e.g. `'^[A-Z]'`
#'
#' @param ignore ignore case, default is `TRUE`
#'
#' @param ... additional arguments
#'
#' @returns `<data.frame>` or `<tibble>`
#'
#' @examples
#' x <- dplyr::tibble(y = 1:10, z = letters[1:10])
#'
#' srchcol(df = x, col = "z", search = "[a|j]")
#'
#' @autoglobal
#'
#' @export
srchcol <- function(df,
                    col,
                    search,
                    ignore = TRUE,
                    ...) {
  dplyr::filter(
    df,
    stringr::str_detect(
      !!rlang::sym(
        col
      ),
      stringr::regex(
        search,
        ignore_case = ignore
      )
    )
  )
}
