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

#' `mean()` with `NA` removal
#'
#' @param x `<numeric>` vector
#'
#' @examples
#' na_mean(c(1, 2, NA))
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
na_mean <- function(x) {
  base::mean(x, na.rm = TRUE)
}

#' `sum()` with `NA` removal
#'
#' @param x `<numeric>` vector
#'
#' @examples
#' na_sum(c(1, 2, NA))
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
na_sum <- function(x) {
  base::sum(x, na.rm = TRUE)
}

#' `sd()` with `NA` removal
#'
#' @param x `<numeric>` vector
#'
#' @examples
#' na_sd(c(1, 2, NA))
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
na_sd <- function(x) {
  stats::sd(x, na.rm = TRUE)
}

#' `median()` with `NA` removal
#'
#' @param x `<numeric>` vector
#'
#' @examples
#' na_med(c(1, 2, NA))
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
na_med <- function(x) {
  stats::median(x, na.rm = TRUE)
}

#' `var()` with `NA` removal
#'
#' @param x `<numeric>` vector
#'
#' @examples
#' na_var(c(1, 2, NA))
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
na_var <- function(x) {
  stats::var(x, na.rm = TRUE)
}

#' `mad()` with `NA` removal
#'
#' Compute the median absolute deviation, i.e., the (lo-/hi-) median of the
#' absolute deviations from the median, and (by default) adjust by a factor for
#' asymptotically normal consistency.
#'
#' @param x `<numeric>` vector
#'
#' @param ... additional arguments to pass to `stats::mad()`
#'
#' @examples
#' na_mad(c(1, 2, NA))
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
na_mad <- function(x, ...) {
  stats::mad(x, na.rm = TRUE, ...)
}

#' `IQR()` with `NA` removal
#'
#' @param x `<numeric>` vector
#'
#' @param ... additional arguments to pass to `stats::IQR()`
#'
#' @examples
#' na_iqr(c(1, 2, NA))
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
na_iqr <- function(x, ...) {
  stats::IQR(x, na.rm = TRUE, ...)
}

#' `range()` with `NA` removal
#'
#' @param ... `<numeric>` vector
#'
#' @examples
#' na_range(c(1, 2, NA))
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
na_range <- function(...) {
  base::range(..., na.rm = TRUE)
}

#' `min()` with `NA` removal
#'
#' @param ... `<numeric>` vector
#'
#' @examples
#' na_min(c(1, 2, NA))
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
na_min <- function(...) {
  base::min(..., na.rm = TRUE)
}

#' `max()` with `NA` removal
#'
#' @param ... `<numeric>` vector
#'
#' @examples
#' na_max(c(1, 2, NA))
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
na_max <- function(...) {
  base::max(..., na.rm = TRUE)
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

#' Create Integer Sequence Beginning at 1
#'
#' @param n `<integer>` Ending number of sequence
#'
#' @returns `<integer>` vector of numbers from 1 to `n`
#'
#' @examples
#' colon(50)
#'
#' colon(-20)
#'
#' colon(0 + 150 - 145)
#'
#' colon(20.9)
#'
#' colon(20.1)
#'
#' @autoglobal
#'
#' @export
colon <- function(n) {

  if (!rlang::is_integerish(n, n = length(n))) {
    rlang::warn(
      message = "`n` has been coerced to `<integer>`.",
      class = "non_int")
  }
  1:n
}

#' Remove duplicates and `NA` values
#'
#' @param x vector
#'
#' @returns vector with duplicates and `NA` values removed
#'
#' @examples
#' c("4", "100", "100", NA)
#'
#' uniq_rmna(c("4", "100", "100", NA))
#'
#' @autoglobal
#'
#' @export
uniq_rmna <- function(x) {
  collapse::funique(collapse::na_rm(x))
}
