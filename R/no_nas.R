#' `mean()` with `NA` removal
#'
#' @param x `<numeric>` vector
#'
#' @examples
#' mean_na(c(1, 2, NA))
#'
#' @autoglobal
#'
#' @export
mean_na <- function(x) {
  base::mean(x, na.rm = TRUE)
}

#' `sum()` with `NA` removal
#'
#' @param x `<numeric>` vector
#'
#' @examples
#' sum_na(c(1, 2, NA))
#'
#' @autoglobal
#'
#' @export
sum_na <- function(x) {
  base::sum(x, na.rm = TRUE)
}

#' `sd()` with `NA` removal
#'
#' @param x `<numeric>` vector
#'
#' @examples
#' sd_na(c(1, 2, NA))
#'
#' @autoglobal
#'
#' @export
sd_na <- function(x) {
  stats::sd(x, na.rm = TRUE)
}

#' `median()` with `NA` removal
#'
#' @param x `<numeric>` vector
#'
#' @examples
#' median_na(c(1, 2, NA))
#'
#' @autoglobal
#'
#' @export
median_na <- function(x) {
  stats::median(x, na.rm = TRUE)
}

#' `var()` with `NA` removal
#'
#' @param x `<numeric>` vector
#'
#' @examples
#' var_na(c(1, 2, NA))
#'
#' @autoglobal
#'
#' @export
var_na <- function(x) {
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
#' mad_na(c(1, 2, NA))
#'
#' @autoglobal
#'
#' @export
mad_na <- function(x, ...) {
  stats::mad(x, na.rm = TRUE, ...)
}

#' `IQR()` with `NA` removal
#'
#' @param x `<numeric>` vector
#'
#' @param ... additional arguments to pass to `stats::IQR()`
#'
#' @examples
#' iqr_na(c(1, 2, NA))
#'
#' @autoglobal
#'
#' @export
iqr_na <- function(x, ...) {
  stats::IQR(x, na.rm = TRUE, ...)
}

#' `range()` with `NA` removal
#'
#' @param ... `<numeric>` vector
#'
#' @examples
#' range_na(c(1, 2, NA))
#'
#' @autoglobal
#'
#' @export
range_na <- function(...) {
  base::range(..., na.rm = TRUE)
}
