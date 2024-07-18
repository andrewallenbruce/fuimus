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
