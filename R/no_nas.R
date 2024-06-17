#' `mean()` with `NA` removal
#'
#' @param x numeric vector
#'
#' @examples
#' mean_na(c(1, 2, NA))
#'
#' @autoglobal
#'
#' @export
mean_na <- function(x) {

  mean(x, na.rm = TRUE)
}

#' `sum()` with `NA` removal
#'
#' @param x numeric vector
#'
#' @examples
#' sum_na(c(1, 2, NA))
#'
#' @autoglobal
#'
#' @export
sum_na <- function(x) {

  sum(x, na.rm = TRUE)
}


#' `sd()` with `NA` removal
#'
#' @param x numeric vector
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
#' @param x numeric vector
#'
#' @examples
#' sd_na(c(1, 2, NA))
#'
#' @autoglobal
#'
#' @export
median_na <- function(x) {

  stats::median(x, na.rm = TRUE)
}

# `var()` with `NA` removal
# `mad()` with `NA` removal
# `IQR` with `NA` removal
