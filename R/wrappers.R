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
#' @export
na_max <- function(...) {
  base::max(..., na.rm = TRUE)
}
