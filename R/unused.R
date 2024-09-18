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
#' @noRd
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
#' @examplesIf FALSE
#'
#' add_ifelse(x = "Practitioner",
#'            df = df,
#'            dfcol = df$mue_service_type,
#'            by = mue_service_type)
#'
#' @autoglobal
#'
#' @noRd
add_ifelse <- function(x, df, dfcol, by) {

  if (vctrs::vec_is_empty(x)) {
    NULL
  } else {
    vctrs::vec_slice(df,
                     vctrs::vec_in(dfcol, x)) |>
      tidyr::nest(.by = {{ by }}) }
}

#' Test if a path is a directory
#'
#' @param x `<chr>` directory path to check
#'
#' @returns named `<lgl>` vector, where the names give the paths. If the given
#'    object does not exist, `NA` is returned.
#'
#' @examples
#' is_directory("C:/")
#'
#' @autoglobal
#'
#' @noRd
is_directory <- function(x) {
  fs::is_dir(x)
}

#' Test if a path is readable
#'
#' @param x `<chr>` vector of one or more paths
#'
#' @returns `<lgl>` vector, with names corresponding to input path.
#'
#' @examples
#' is_readable("D:/")
#'
#' @autoglobal
#'
#' @noRd
is_readable <- function(x) {
  fs::file_exists(x)
}


#' Count missing values
#'
#' @param df A `<data.frame>` or `<tibble>`
#'
#' @param group_vars A `<character>` or `<symbol>` vector of the variables to
#'   group by
#'
#' @param x_var A `<character>` or `<symbol>`  vector of the variable to count
#'   missing values for
#'
#' @returns A `<data.frame>` or `<tibble>` with the count of missing values
#'
#' @examples
#' dplyr::tibble(x = 1:10,
#'               y = 1:10,
#'               z = letters[1:10]) |>
#'               count_missing(z, x)
#' @autoglobal
#'
#' @noRd
count_missing <- function(df,
                          group_vars,
                          x_var) {
  df |>
    dplyr::group_by(
      dplyr::pick({{ group_vars }})) |>
    dplyr::summarize(
      n_miss = sum(is.na({{ x_var }})),
      .groups = "drop"
    )
}

#' Show missing values
#'
#' @param df A `<data.frame>` or `<tibble>`
#'
#' @param group_vars A `<character>` or `<symbol>` vector of the variables to
#'   group by
#'
#' @param summary_vars A `<character>` or `<symbol>` vector of the variables to
#'   summarize; default is [dplyr::everything()]
#'
#' @examples
#' dplyr::tibble(x = 1:10,
#'               y = 1:10,
#'               z = c(letters[1:10])) |>
#'               show_missing(z)
#'
#' @autoglobal
#'
#' @noRd
show_missing <- function(df,
                         group_vars,
                         summary_vars = dplyr::everything()) {
  df |>
    dplyr::group_by(
      dplyr::pick(
        {{ group_vars }}
      )
    ) |>
    dplyr::summarize(
      dplyr::across(
        {{ summary_vars }}, \(x) sum(is.na(x)
        )
      ),
      .groups = "drop"
    ) |>
    dplyr::select(
      dplyr::where(\(x) any(x > 0)
      )
    )
}
