#' Chop Vector by Group
#'
#' @param v `<character>` vector
#'
#' @param g `<integer>` group
#'
#' @returns `<list>`
#'
#' @examples
#' (v <- c("222", "280", "3020", "8690", "G0294", "G8126"))
#'
#' (g <- sample(1:2, size = length(v), replace = TRUE))
#'
#' gchop(v, g)
#'
#' @autoglobal
#'
#' @keywords helpers
#'
#' @family tidyverse
#'
#' @export
gchop <- \(v, g) { vctrs::vec_chop(x = v, sizes = vctrs::vec_run_sizes(g)) }

#' Remove empty rows and columns
#'
#' @param df data frame
#'
#' @param ... additional arguments to pass to `janitor::remove_empty()`
#'
#' @examples
#' dplyr::tibble(
#'   x = c(1, 2, NA),
#'   y = c(NA, NA, NA)) |>
#'   remove_quiet()
#'
#' @autoglobal
#'
#' @keywords helpers
#'
#' @export
remove_quiet <- \(df, ...) { janitor::remove_empty(df, which = c("rows", "cols"), ...) }

#' Coerce glue() output to character
#'
#' @param ... dots to pass to glue function
#'
#' @returns `<chr>` vector
#'
#' @autoglobal
#'
#' @keywords helpers
#'
#' @family tidyverse
#'
#' @export
glue_chr <- \(...) { as.character(glue::glue(..., .envir = parent.frame(1))) }

#' Coerce glue_data() output to character
#'
#' @param ... dots to pass to glue function
#'
#' @param .x `<vec>` vector to pass to glue_data()
#'
#' @returns `<chr>` vector
#'
#' @autoglobal
#'
#' @keywords helpers
#'
#' @family tidyverse
#'
#' @export
glue_data_chr <- \(.x, ...) { as.character(glue::glue_data(.x = .x, ..., .envir = parent.frame(1))) }

#' Split a `<tibble>` by Groups with Named List Output
#'
#' This function takes a `<tibble>`, groups it by one or more variables, and
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
#' (x <- dplyr::tibble(
#'   zip = c("Data_Weekly.zip",
#'         "Data_Weekly.zip",
#'         "Data_April.zip",
#'         "Deactivated.zip"),
#'   file = c(
#'     "npidata.csv",
#'     "npidata2.csv",
#'     "endpoint.csv",
#'     "Deactivated.xlsx")))
#'
#' named_group_split(x, zip)
#'
#' @autoglobal
#'
#' @keywords helpers
#'
#' @family tidyverse
#'
#' @export
named_group_split <- \(df, ...) {

  grouped <- dplyr::group_by(df, ...)

  names <- rlang::inject(
    paste(
      !!!dplyr::group_keys(grouped),
      sep = "_"))

  grouped |>
    dplyr::group_split() |>
    rlang::set_names(names) |>
    as.list()
}

#' Search a data frame column by string
#'
#' @param data `<data.frame>` or `<tibble>`
#'
#' @param column `<chr>` string, column to search
#'
#' @param search `<chr>` string to search for, can be a regex
#'
#' @param ignore `<lgl>` ignore case, default is `TRUE`
#'
#' @param ... additional arguments
#'
#' @returns `<data.frame>` or `<tibble>`
#'
#' @examples
#' (x <- dplyr::tibble(y = 1:10, z = letters[1:10]))
#'
#' search_for(data = x, column = "z", search = "[a|j]")
#'
#' @autoglobal
#'
#' @keywords helpers
#'
#' @family tidyverse
#'
#' @export
search_for <- function(data,
                       column,
                       search,
                       ignore = TRUE,
                       ...) {
  dplyr::filter(
    data,
    stringr::str_detect(
      !!rlang::sym(column),
      stringr::regex(
        search,
        ignore_case = ignore)
    )
  )
}

#' Coerce all columns to type character
#'
#' @param data `<data.frame>`
#'
#' @returns `<data.frame>` with columns coerced to character
#'
#' @examples
#' (x <- dplyr::tibble(int = c(1:9, NA_integer_), chr = letters[1:10], date = Sys.Date()))
#'
#' columns_to_character(x)
#'
#' @autoglobal
#'
#' @keywords helpers
#'
#' @family tidyverse
#'
#' @export
columns_to_character <- function(data) {

  stopifnot(inherits(data, "data.frame"))

  dplyr::mutate(
    data,
    dplyr::across(
      dplyr::everything(),
      as.character))
}

#' Return data.frame of column types
#'
#' @param data `<data.frame>`
#'
#' @returns `<data.frame>` or `<tibble>`
#'
#' @examples
#' (x <- dplyr::tibble(int = c(1:9, NA_integer_), chr = letters[1:10], date = Sys.Date()))
#'
#' data_types(x)
#'
#' @export
#'
#' @autoglobal
data_types <- function(data) {
  dplyr::tibble(
    column = names(data),
    type = purrr::map_chr(data, vctrs::vec_ptype_full),
    missing = purrr::map_int(data, \(x) collapse::fsum(na(x)))
  )
}

#' Pivot data frame to long format for easy printing
#'
#' @param data `<data.frame>`
#'
#' @returns a `<data.frame>` or `<tibble>` in long format
#'
#' @examples
#' (x <- dplyr::tibble(int = c(1:9, NA_integer_), chr = letters[1:10], date = Sys.Date()))
#'
#' display_long(x)
#'
#' @autoglobal
#'
#' @export
display_long <- function(data) {

  tidyr::pivot_longer(
      data = columns_to_character(data),
      cols = dplyr::everything())
}

#' Combine multiple columns into one
#'
#' @param data `<data.frame>` or `<tibble>`
#'
#' @param name new column name, unquoted, default is `combined`
#'
#' @param columns `<chr>` vector of columns to combine
#'
#' @param sep separator between combined row data, default is `"-"`
#'
#' @returns A `<tibble>` with combined columns
#'
#' @examples
#' x <- forager_data()[-5]
#'
#' x[1, 2] <- ""
#'
#' x
#'
#' combine(
#'   data = x,
#'   name = payer,
#'   columns = c('claim_id', 'payer'))
#'
#' @autoglobal
#'
#' @export
combine <- function(data, name = combined, columns, sep = "-") {

  x <- tidyr::unite(
    data,
    col = {{ name }},
    dplyr::any_of(columns),
    remove = TRUE,
    na.rm = TRUE,
    sep = sep)

    dplyr::mutate(
      x,
      {{ name }} := dplyr::na_if(
        {{ name }}, ""))
}

#' Count and calculate proportion of each group
#'
#' @param df A `<data.frame>` or `<tibble>`
#'
#' @param var A `<character>` or `<symbol>` specifying the column to count
#'
#' @param sort A `<logical>` indicating whether to sort the output by frequency,
#'   default is `FALSE`
#'
#' @param na.rm A `<logical>` indicating whether to remove missing values from
#'   the count, default is `FALSE`
#'
#' @examples
#' forager_data() |>
#'   count_prop(payer, sort = TRUE)
#'
#' @autoglobal
#'
#' @export
count_prop <- function(df, var, sort = FALSE, na.rm = FALSE) {

  dplyr::count(df, {{ var }}, sort = sort) |>
    dplyr::mutate(prop = n / sum(n, na.rm = na.rm))
}

#' Count and calculate proportion of each group
#'
#' @param df A `<data.frame>` or `<tibble>`
#'
#' @param rows A `<character>` or `<symbol>` specifying the rows to count
#'
#' @param cols A `<character>` or `<symbol>` specifying the columns to count
#'
#' @param sort A `<logical>` indicating whether to sort the output by frequency,
#'   default is `FALSE`
#'
#' @param na.rm A `<logical>` indicating whether to remove missing values from
#'   the count, default is `FALSE`
#'
#' @examples
#' forager_data(10) |>
#'   count_prop_multi(
#'   c(ins_class),
#'   payer,
#'   sort = TRUE)
#'
#' @autoglobal
#'
#' @export
count_prop_multi <- function(df, rows, cols, sort = FALSE, na.rm = FALSE) {

  dplyr::count(df, dplyr::pick(c({{ rows }}, {{ cols }})), sort = sort) |>
    dplyr::mutate(prop = n / sum(n, na.rm = na.rm))
}

#' Count rows and columns and pivot to wide format
#'
#' @param df A `<data.frame>` or `<tibble>`
#'
#' @param rows A `<character>` or `<symbol>` specifying the rows to count
#'
#' @param cols A `<character>` or `<symbol>` specifying the columns to count
#'
#' @examples
#' fuimus:::forager_data(10) |>
#'   count_wide(c(ins_class), payer)
#'
#' @autoglobal
#'
#' @export
count_wide <- function(df, rows, cols) {

  df |>
    dplyr::count(
      dplyr::pick(
        c({{ rows }}, {{ cols }})
      )
    ) |>
    tidyr::pivot_wider(
      names_from  = {{ cols }},
      values_from = n,
      names_sort  = TRUE,
      values_fill = 0
    )
}

#' Summary statistics
#'
#' @param df data frame
#'
#' @param condition filter condition, i.e. `patient == "new"`
#'
#' @param group_vars variables to group by, i.e. `c(specialty, state, hcpcs, cost)`
#'
#' @param summary_vars variables to summarise, i.e. `c(min, max, mode, range)`
#'
#' @param arr column to arrange data by, i.e. `cost`
#'
#' @param digits Number of digits to round to, default is 3
#'
#' @returns A `<tibble>` with the summarized data
#'
#' @examples
#' x <- dplyr::tibble(
#'    provider = sample(c("A", "B", "C"), size = 200, replace = TRUE),
#'    city = sample(c("ATL", "NYC"), size = 200, replace = TRUE),
#'    charges = sample(1000:2000, size = 200),
#'    payment = sample(1000:2000, size = 200))
#'
#' summary_stats(
#'    x,
#'    condition    = city == "ATL",
#'    group_vars   = provider,
#'    summary_vars = c(charges, payment),
#'    arr          = provider)
#' @autoglobal
#'
#' @export
summary_stats <- function(df,
                          condition = NULL,
                          group_vars = NULL,
                          summary_vars = NULL,
                          arr = NULL,
                          digits = 3) {

  df |>
    dplyr::filter({{ condition }}) |>
    dplyr::summarise(
      dplyr::across({{ summary_vars }},
                    list(median = \(x) stats::median(x, na.rm = TRUE),
                         mean = \(x) mean(x, na.rm = TRUE),
                         sd = \(x) stats::sd(x, na.rm = TRUE)),
                    .names = "{.col}_{.fn}"),
      n = dplyr::n(),
      .by = ({{ group_vars }})) |>
    dplyr::arrange(dplyr::desc({{ arr }})) |>
    dplyr::mutate(dplyr::across(
      dplyr::where(is.double), ~roundup(., d = digits)))
}
