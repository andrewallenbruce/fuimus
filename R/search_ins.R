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
#' x <- dplyr::tibble(y = 1:10,
#'                    z = letters[1:10])
#'
#' search_in(df = x,
#'           dfcol = x$z,
#'           search = c("a", "j"))
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

#' Search in data frame column if search term is not `NULL`
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
#' x <- dplyr::tibble(
#'      y = 1:10,
#'      z = letters[1:10])
#'
#' search_in_if(df = x,
#'              dfcol = x$z,
#'              search = c("a", "j"))
#'
#' search_in_if(df = x,
#'              dfcol = x$z,
#'              search = NULL)
#'
#' @autoglobal
#'
#' @export
search_in_if <- function(df, dfcol, search) {

  if (!is.null(search)) {

    vctrs::vec_slice(df,
    vctrs::vec_in(dfcol,
    collapse::funique(search)))

  } else { df }

}

#' Search in data frame column if search term is not `NULL`
#'
#' @param df A `<data.frame>` or `<tibble>`
#'
#' @param dfcol A `<character>` or `<symbol>` specifying the column to search in
#'
#' @param search A `<character>` or `<symbol>` specifying the search term
#'
#' @param args A `<character>` vector of argument options; default is `NULL`
#'
#' @param multiple A `<logical>` indicating if multiple `search` args are
#'   allowed; default is `FALSE`
#'
#' @returns A `<data.frame>` or `<tibble>`
#'
#' @examples
#' x <- dplyr::tibble(y = 1:10, z = letters[1:10])
#'
#' search_in_if_args(df = x, dfcol = x$z, search = c("a", "j"))
#'
#' search_in_if_args(df = x, dfcol = x$z, search = NULL)
#'
#' search_in_if_args(df = x,
#'                   dfcol = x$z,
#'                   search = c("a", "j"),
#'                   args = c("a", "j"),
#'                   multiple = TRUE)
#'
#' try(search_in_if_args(df = x,
#'                       dfcol = x$z,
#'                       search = c("a", "j"),
#'                       args = c("a", "z"),
#'                       multiple = FALSE))
#'
#' @autoglobal
#'
#' @export
search_in_if_args <- function(df,
                              dfcol,
                              search,
                              args = NULL,
                              multiple = FALSE) {

  if (!is.null(search)) {

    if (!is.null(args)) {
      search <- rlang::arg_match(
        arg      = search,
        values   = args,
        multiple = multiple)
    }

    vctrs::vec_slice(df,
    vctrs::vec_in(dfcol,
    collapse::funique(search)))

  } else {
    df
  }
}

#' Search in data frame column if search term is not `NULL`
#'
#' @param x A `<character>` or `<symbol>` specifying the search term
#'
#' @param df A `<data.frame>` or `<tibble>`
#'
#' @param dfcol A `<character>` or `<symbol>` specifying the column to search in
#'
#' @param by A `<character>` or `<symbol>` specifying the column to nest by
#'
#' @returns A `<data.frame>` or `<tibble>`
#'
#' @examplesIf interactive()
#'
#' df <- northstar::get_mue_edits()
#'
#' add_ifelse(x = "Practitioner",
#'            df = df,
#'            dfcol = df$mue_service_type,
#'            by = mue_service_type)
#'
#' @keywords internal
#'
#' @autoglobal
#'
#' @export
add_ifelse <- function(x, df, dfcol, by) {

  if (vctrs::vec_is_empty(x)) {
    NULL
  } else {
    vctrs::vec_slice(df,
    vctrs::vec_in(dfcol, x)) |>
    tidyr::nest(.by = {{ by }}) }
}
