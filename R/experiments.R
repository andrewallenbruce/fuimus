#' Determine a common ordering from a set of vectors
#'
#' @param ... Input vectors
#'
#' @param .strict If `TRUE`, an error will be thrown if no common ordering
#'   exists. If `FALSE`, a warning will be thrown and the output will be
#'   returned. Note that to use the output in such cases, you may want to
#'   call `unique()` to remove any duplicates.
#'
#' @returns `<character>` vector of `n` NPIs
#'
#' @examplesIf FALSE
#'
#' find_common_order(
#'   c(1, 3, 4, 7, 8),
#'   c(2, 3, 4, 8, 9),
#'   c(4, 5, 6, 7, 9)
#' )
#'
#' find_common_order(
#'   c("bananas", "oranges", "apples"),
#'   c("oranges", "avocados", "apples"),
#'   c("apples", "mangos", "lemons")
#' )
#'
#' find_common_order(
#'   c("foo", "bar"),
#'   c("bar", "baz"),
#'   c("baz", "foo")
#' )
#' @autoglobal
#'
#' @export
find_common_order <- function(..., .strict = TRUE) {


  vecs <- vec_original <- rlang::list2(...) |>
    purrr::map(rle) |>
    purrr::map("values")

  out  <- NULL

  while (length(vecs) > 0L) {

    # Get the first element from each vector

    first_elements <- vecs |>
      purrr::map_vec(1L, .ptype = vecs[[1L]]) |>
      unique()

    # Get the maximum position of each
    # candidate element across all vectors

    max_positions <- first_elements |>
      purrr::map_int(
        function(e)
          max(
            purrr::map_int(
              vecs, ~ max(which(. == e) %or% 1L))))

    # Choose the one with the lowest
    # maximum position as the next in order

    next_element <- first_elements[max_positions == min(max_positions)][1]
    out <- c(out, next_element)

    # Remove the next element from the start of each vector
    vecs <- vecs |>
      purrr::map_if(~ .[1L] == next_element, ~ .[-1L]) |>
      purrr::compact()
  }

  # If any element appears in the output twice
  # then no universal ordering exists
  dupes <- tapply(out, out, length, simplify = FALSE) |>
    purrr::keep(~ . > 1L) |>
    names() |>
    unique()

  if (length(dupes) == 0L) return(out)

  # Error if no universal ordering exists
  names(vec_original) <- if (!is.null(...names())) {
    purrr::map_chr(...names(), function(x) cli::format_inline("{.arg {x}}"))
  } else {
    paste("Argument", seq_len(...length()))
  }
  dupe_positions <- which(out == dupes[1])
  bad_elements   <- out[seq(dupe_positions[1L], dupe_positions[2L])]
  info           <- purrr::map2_chr(
    bad_elements[-length(bad_elements)], bad_elements[-1L],
    function(x, y) {
      vec_name <- vec_original |>
        purrr::map(intersect, c(x, y)) |>
        purrr::keep(identical, c(x, y)) |>
        names() |>
        _[1]

      cli::format_inline("{vec_name}: {.val {x}} comes before {.val {y}}")
    }
  )

  throw <- if (.strict) cli::cli_abort else cli::cli_warn

  throw(
    c(
      "No universal ordering exists",
      purrr::set_names(info, c(rep("v", length(info) - 1L), "x")),
      if (length(dupes) > 1L)
        c(i = "And {length(dupes) - 1L} other similar cases")))
  out
}

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
  sf_conv(
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

  npi <- as_chr(npi)
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
