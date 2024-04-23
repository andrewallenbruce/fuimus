#' Pad numbers with zeroes
#'
#' @param x `<dbl>` vector of numbers
#'
#' @param digits `<int>` single integer; the number of zeroes to pad `x` with.
#'   If left `NULL` (the default), will use the result of `max(nchar(x))`
#'
#' @returns `<chr>` vector of padded numbers
#'
#' @examples
#' pad_number(c(56, 2584, 010, 912, 12222))
#'
#' pad_number(c(56, 2584, 010, 912, 00012222), digits = 10L)
#'
#' @autoglobal
#'
#' @export
pad_number <- function(x, digits = NULL){

  if (rlang::is_null(digits)) {

    digits <- max(
      nchar(x),
      na.rm = TRUE
    )
  }

  stopifnot(
    rlang::is_scalar_integer(digits),
    digits > 1
  )

  x <- as.character(x)
  rpt <- digits - nchar(x)

  rpt <- dplyr::if_else(
    rpt < 0L,
    0L,
    rpt
  )

  paste0(
    sapply(
      rpt,
      FUN = function(x)
        paste0(
          rep("0", times = x), collapse = "")), x
  )
}
