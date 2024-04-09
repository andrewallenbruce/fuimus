#' Search in data frame
#'
#' @param df A `<data.frame>` or `<tibble>`
#'
#' @param dfcol A `<character>` or `<symbol>` specifying the column to search in
#'
#' @param search A `<character>` or `<symbol>` specifying the search term
#'
#' @returns A `<data.frame>` or `<tibble>`
#'
#' @examples
#' x <- dplyr::tibble(y = 1:10, z = letters[1:10])
#'
#' search_in(df = x, dfcol = x$z, search = c("a", "j"))
#'
#' @autoglobal
#'
#' @export
search_in <- function(df, dfcol, search) {
  vctrs::vec_slice(
    df,
    vctrs::vec_in(
      dfcol,
      collapse::funique(
        search
      )
    )
  )
}
