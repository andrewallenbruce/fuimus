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
#' @family mock
#'
#' @export
random_npi_generator <- \(n) {
  sf_convert(
    replicate(
      n = n,
      expr = paste0(
        c(sample(1:2, 1, replace = TRUE),
          sample(0:9, 9, replace = TRUE)),
        collapse = "")))
}

#' Check length of `x` is 1
#'
#' @param x vector
#'
#' @autoglobal
#'
#' @keywords checks
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

#' Check that `x` contains numbers only
#'
#' @param x vector
#'
#' @autoglobal
#'
#' @keywords checks
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
#' @keywords checks
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
#' @keywords checks
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

#' Validate NPIs (v1)
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
#' @keywords checks
#'
#' @export
is_valid_npi <- function(npi) {

  npi <- as_character(npi)
  check_length(npi)
  check_chars_numeric(npi)
  check_nchars_10(npi)
  check_first_char(npi)

  p  <- \(...) paste0(...)
  s  <- \(x) unlist(strsplit(x, ""), use.names = FALSE)
  ix <- c(1, 3, 5, 7, 9)

  id     <- as.integer(rev(s(npi)[1:9]))
  id[ix] <- id[ix] * 2
  id[ix] <- ifelse(id[ix] > 9, id[ix] - 9, id[ix])

  id   <- sum(id) + 24
  ck   <- (ceiling(id / 10) * 10) - id
  test <- p(substr(npi, 1, 9), ck)

  identical(test, npi)
}

#' Validate NPIs (v2)
#'
#' @param x `<character>` Number of NPIs to generate
#'
#' @returns `<logical>` vector
#'
#' @examples
#' is_valid_npi2("1043477615")
#'
#' is_valid_npi2("1234567891")
#'
#' x <- random_npi_generator(100)
#'
#' x[collapse::whichv(purrr::map_lgl(x, is_valid_npi2), TRUE)]
#'
#' @autoglobal
#'
#' @keywords checks
#'
#' @export
is_valid_npi2 <- function(x) {

  stopifnot(sf_detect(x, "^[12][0-9]{9}$"))

  smash <- \(...) paste0(..., collapse = "")
  splat <- \(x)   unlist(strsplit(x, ""), use.names = FALSE)
  idx   <- c(1, 3, 5, 7, 9)

  id <- as.integer(cheapr::cheapr_rev(splat(x)[1:9]))

  id[idx] <- id[idx] * 2

  id[idx] <- iif_else(id[idx] > 9, id[idx] - 9, id[idx])

  id   <- sum(id) + 24
  ck   <- (ceiling(id / 10) * 10) - id
  test <- smash(sf_sub(x, start = 1, stop = 9), ck)

  identical(test, x)
}

#' Generate Data for Testing
#'
#' @param year_seq sequence of years, e.g. `2010:2020`
#'
#' @returns A [tibble][tibble::tibble-package] containing the search results.
#'
#' @examples
#' provider_data(2010:2020)
#'
#' @autoglobal
#'
#' @family mock
#'
#' @export
provider_data <- function(year_seq) {

  lng <- length(year_seq) * 2

  vctrs::vec_rbind(
    dplyr::tibble(
      year = {{ year_seq }},
      group = "A"),
    dplyr::tibble(
      year = {{ year_seq }},
      group = "B")
  ) |>
    dplyr::mutate(
      net_payment = sample(
        10000:20000, lng
      )
    )
}

#' Generate mock coding/billing data frame
#'
#' @param rows number of rows to generate; default is 10
#'
#' @param unnest a logical indicating whether to unnest the dates column;
#'   default is `FALSE`
#'
#' @returns A [tibble][tibble::tibble-package]
#'
#' @examples
#' forager_data(rows = 5)
#'
#' @autoglobal
#'
#' @family mock
#'
#' @export
forager_data <- function(rows = 10, unnest = FALSE){


  x <- dplyr::tibble(
    claim_id        = as.character(wakefield::id(n = rows)),
    date_of_service = wakefield::date_stamp(
      n = rows,
      start         = lubridate::today() - lubridate::dyears(2),
      random        = TRUE
    ),
    payer           = fixtuRes::set_vector(
      rows,
      set           = c(
        "Medicare",
        "Medicaid",
        "Cigna",
        "Humana",
        "UnitedHealth",
        "Anthem",
        "BCBS",
        "Centene"
      )
    ),
    ins_class       = fixtuRes::set_vector(rows, set = c("Primary", "Secondary")),
    balance         = as.double(wakefield::income(n = rows, digits = 2) / 300)
  ) |>
    dplyr::mutate(
      date_of_service      = lubridate::as_date(date_of_service),
      date_of_release      = date_of_service + round(abs(stats::rnorm(
        length(date_of_service), 11, 4
      ))),
      date_of_submission   = date_of_release + round(abs(stats::rnorm(
        length(date_of_release), 2, 2
      ))),
      date_of_acceptance   = date_of_submission + round(abs(stats::rnorm(
        length(date_of_submission), 3, 2
      ))),
      date_of_adjudication = date_of_acceptance + round(abs(stats::rnorm(
        length(date_of_acceptance), 30, 3
      )))
    ) |>
    tidyr::nest(dates = tidyr::contains("date"))

  if (unnest)
    x <- tidyr::unnest_wider(x, dates)

  return(x)
}
