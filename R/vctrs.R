#' Is Vector Empty?
#'
#' @param x vector
#'
#' @returns `<logical>`
#'
#' @examples
#' empty(character(0))
#'
#' empty(c())
#'
#' empty(NULL)
#'
#' empty(NA)
#'
#' empty(list())
#'
#' empty(list(character(0)))
#'
#' empty(list(x = character(0)))
#'
#' @autoglobal
#'
#' @family vctrs
#'
#' @export
empty <- \(x) vctrs::vec_is_empty(x)

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
#' @family vctrs
#'
#' @export
gchop <- \(v, g) vctrs::vec_chop(x = v, sizes = vctrs::vec_run_sizes(g))

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
#' @family vctrs
#'
#' @export
null_if_empty <- \(x) if (empty(x)) NULL else x

#' Search in data frame
#'
#' @param data A `<data.frame>` or `<tibble>`
#'
#' @param column column to search; of the form `data[["column"]]` or
#'   `data$column`
#'
#' @param what string or vector to search for in `column`
#'
#' @returns `<data.frame>` or `<tibble>` with rows containing `what`
#'
#' @examples
#' x <- dplyr::tibble(y = 1:10,
#'                    z = letters[1:10])
#'
#' search_in_impl(data = x,
#'                column = x[["z"]],
#'                what = c("a", "j"))
#'
#' @autoglobal
#'
#' @keywords programming
#'
#' @family vctrs
#'
#' @export
search_in_impl <- function(data, column, what) {
  vctrs::vec_slice(
    data,
    vctrs::vec_in(
      column,
      uniq(what)))
}

#' Search in data frame column if search term is not `NULL`
#'
#' @param data `<data.frame>` or `<tibble>`
#'
#' @param column column to search; of the form `data[["column"]]` or
#'   `data$column`
#'
#' @param what string or vector to search for in `column`
#'
#' @returns If `what` is not `NULL`, `<data.frame>` or `<tibble>` with rows
#'   containing `what`
#'
#' @examples
#' x <- dplyr::tibble(
#'      y = 1:10,
#'      z = letters[1:10])
#'
#' search_in(data = x,
#'           column = x[["z"]],
#'           what = c("a", "j"))
#'
#' search_in(data = x,
#'           column = x[["z"]],
#'           what = NULL)
#'
#' @autoglobal
#'
#' @keywords programming
#'
#' @family vctrs
#'
#' @export
search_in <- function(data, column, what) {

  if (is.null(what)) return(data)

    search_in_impl(data, column, what)
}
