#' Return NULL if the input is empty, otherwise return the input
#'
#' @param x vector
#'
#' @returns vector or `NULL`
#'
#' @examples
#' null_if_empty(c())
#'
#' null_if_empty(c(1))
#'
#' @autoglobal
#'
#' @export
null_if_empty <- function(x) {
  if (vctrs::vec_is_empty(x)) NULL else x
}

#' Split a `tibble` by Groups with Named List Output
#'
#' This function takes a table, groups it by one or more variables, and then
#' splits the grouped data into a list. The resulting list has names derived
#' from the unique combinations of the grouping variables.
#'
#' @param df `<tibble>` or `<data.frame>` to split
#'
#' @param ... One or more unquoted variables by which to group and then split
#'   `df`. Variables can be separated by commas.
#'
#' @returns named `<list>` of tibbles, the names of which are derived from the
#'   unique combinations of grouping variables, separated by "_".
#'
#' @examples
#' x <- dplyr::tibble(
#'   zip = c("Data_Weekly.zip",
#'         "Data_Weekly.zip",
#'         "Data_April.zip",
#'         "Deactivated.zip"),
#'   file = c(
#'     "npidata.csv",
#'     "npidata2.csv",
#'     "endpoint.csv",
#'     "Deactivated.xlsx"))
#'
#' x
#'
#' named_group_split(x, zip)
#'
#' @autoglobal
#'
#' @export
named_group_split <- function(df, ...) {

  grouped <- dplyr::group_by(df, ...)

  names <- rlang::inject(
    paste(
      !!!dplyr::group_keys(grouped),
      sep = "_")
  )

  grouped |>
    dplyr::group_split() |>
    rlang::set_names(names) |>
    as.list()
}

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
