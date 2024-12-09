#' Remove empty rows and columns
#'
#' @param df data frame
#'
#' @param ... additional arguments to pass to `janitor::remove_empty()`
#'
#' @examples
#' x <- dplyr::tibble(
#'   x = c(1, 2, NA),
#'   y = c(NA, NA, NA))
#'
#' x
#'
#' remove_quiet(x)
#'
#' @autoglobal
#'
#' @export
remove_quiet <- function(df, ...) {

  janitor::remove_empty(
    df,
    which = c("rows", "cols"),
    ...
  )
}

#' Round a numeric vector up to the nearest decimal place
#'
#' Wrapper around [janitor::round_half_up()]
#'
#' @param x `<dbl>` A numeric vector
#'
#' @param digits `<int>` Number of decimal places to round to
#'
#' @returns a numeric vector rounded up to the nearest decimal place
#'
#' @autoglobal
#'
#' @export
roundup <- function(x, digits = 2) {

  janitor::round_half_up(
    x,
    digits = digits
  )
}

#' Wrapper for `as.character(glue::glue(x))`
#'
#' @param ... dots to pass to glue function
#'
#' @returns `<chr>` vector
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
glue_chr <- function(...) {
  as.character(
    glue::glue(
      ...,
      .envir = parent.frame(1)) )
}

#' Wrapper for `as.character(glue::glue_data(x))`
#'
#' @param ... dots to pass to glue function
#'
#' @param .x `<vec>` vector to pass to glue_data()
#'
#' @returns `<chr>` vector
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
glue_data_chr <- function(.x, ...) {
  as.character(
    glue::glue_data(
      .x = .x,
      ...,
      .envir = parent.frame(1)))
}
