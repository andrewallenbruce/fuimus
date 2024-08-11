#' Generate random NPIs
#'
#' @param n `<integer>` Number of NPIs to generate
#'
#' @returns `<character>` vector of `n` NPIs
#'
#' @examples
#' random_npi_generator(10)
#'
#' x <- random_npi_generator(100)
#'
#' x[collapse::whichv(purrr::map_lgl(x, is_valid_npi), TRUE)]
#'
#' @autoglobal
#'
#' @export
random_npi_generator <- \(n) {
  replicate(
    n = n,
    expr = paste0(
      c(sample(1:2, 1, replace = TRUE),
        sample(0:9, 9, replace = TRUE)),
      collapse = ""))
}

#' Validate NPIs
#'
#' @param npi `<character>` Number of NPIs to generate
#'
#' @returns `<logical>` vector
#'
#' @examples
#' is_valid_npi(1043477615)
#'
#' is_valid_npi(1234567891)
#'
#' x <- random_npi_generator(100)
#'
#' x[collapse::whichv(purrr::map_lgl(x, is_valid_npi), TRUE)]
#'
#' @autoglobal
#'
#' @export
is_valid_npi <- function(npi) {

  npi <- numeric_to_char(npi)
  check_length(npi)
  check_chars_numeric(npi)
  check_nchars_10(npi)
  check_first_char(npi)

  p  <- \(...) paste0(...)
  s  <- \(x) unlist(strsplit(x, ""), use.names = FALSE)
  ix <- c(1, 3, 5, 7, 9)

  id     <- as.numeric(rev(s(npi)[1:9]))
  id[ix] <- id[ix] * 2
  id[ix] <- ifelse(id[ix] > 9, id[ix] - 9, id[ix])

  id   <- sum(id) + 24
  ck   <- (ceiling(id / 10) * 10) - id
  test <- p(substr(npi, 1, 9), ck)

  identical(test, npi)
}

#' Check length of `x` is 1
#'
#' @param x vector
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
check_length <- function(x) {

  arg  <- rlang::caller_arg(x)
  call <- rlang::caller_env()

  if (length(x) != 1) {
    cli::cli_abort(
      "{.arg {arg}} must be of length 1.",
      arg = arg,
      call = call,
      class = "check_length"
    )
  }
}

#' Coerce `<numeric>` vector to `<character>`
#' @param x vector
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
numeric_to_char <- function(x) {
  if (!rlang::is_character(x)) {
    as.character(x)
  } else {
    x
  }
}

#' Check that `x` contains numbers only
#'
#' @param x vector
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
check_chars_numeric <- function(x) {

  arg  <- rlang::caller_arg(x)
  call <- rlang::caller_env()

  if (!stringfish::sf_grepl(x, "^[[:digit:]]+$")) {
    cli::cli_abort(
      "An {.arg {arg}} must contain numbers only.",
      arg = arg,
      call = call,
      class = "check_chars_numeric"
    )
  }
}

#' Check that `x` is 10 characters long
#'
#' @param x vector
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
check_nchars_10 <- function(x) {

  arg  <- rlang::caller_arg(x)
  call <- rlang::caller_env()

  if (stringfish::sf_nchar(x) != 10L) {
    cli::cli_abort(
      "An {.arg {arg}} must be 10 characters long.",
      arg = arg,
      call = call,
      class = "check_nchars_10"
    )
  }
}

#' Check that `x` begin with 1 or 2
#'
#' @param x vector
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
check_first_char <- function(x) {

  arg  <- rlang::caller_arg(x)
  call <- rlang::caller_env()

  if (!stringfish::sf_substr(x, 1, 1) %in% c("1", "2")) {
    cli::cli_abort(
      "An {.arg {arg}} must start with a 1 or 2.",
      arg = arg,
      call = call,
      class = "check_first_char"
    )
  }
}
