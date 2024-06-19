#' Clean character vector of numbers
#'
#' @param x `<chr>` vector of numbers
#'
#' @returns a `<dbl>` vector of numbers
#'
#' @examples
#' clean_number(c("20%", "21,125,458", "$123"))
#'
#' @autoglobal
#'
#' @export
clean_number <- function(x) {

  is_pct <- stringr::str_detect(x, "%")

  x <- x |>
    stringr::str_remove_all("%") |>
    stringr::str_remove_all(",") |>
    stringr::str_remove_all(stringr::fixed("$")) |>
    as.numeric(x)

  dplyr::if_else(is_pct, x / 100, x)
}

#' Invert a named vector
#'
#' @param x A named vector
#'
#' @returns A named vector with names and values inverted
#'
#' @examples
#' invert_named(x = c(name = "element"))
#'
#' invert_named(x = c(element = "name"))
#'
#' @autoglobal
#'
#' @export
invert_named <- function(x) {

  stopifnot(
    "Input must be a named vector" = !is.null(names(x)))

  rlang::set_names(names(x), unname(x))
}

#' Regular expression for matching month names
#'
#' @returns `<chr>` string  of a regex for matching month names
#'
#' @examples
#' months_regex()
#'
#' @autoglobal
#'
#' @export
months_regex <- function() {
  single_line_string(
    "(Jan(?:uary)?|Feb(?:ruary)?|Mar(?:ch)?|Apr(?:il)?|May|Jun(?:e)?|
     Jul(?:y)?|Aug(?:ust)?|Sep(?:tember)?|Oct(?:ober)?|Nov(?:ember)?|
     Dec(?:ember)?)\\s+(\\d{1,2})\\,\\s+(\\d{4})")
}

#' Convert various character strings to `NA`
#'
#' @param x `<chr>` vector to convert
#'
#' @returns `<chr>` vector with converted `NA` values
#'
#' @examples
#' na_if_common(x = c(" ", "*", "--", "N/A", "", "A", "B"))
#'
#' @autoglobal
#'
#' @export
na_if_common <- function(x) {

  dplyr::case_match(
    x,
    c("", " ", "*", "--", "N/A") ~ NA_character_,
    .default = x)

}

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

#' Generate a sequence of numbers with a new prefix
#'
#' @param n `<int>` Numeric sequence to generate
#'
#' @param new `<chr>` New prefix
#'
#' @param between `<chr>` Separator between `new` and `old`, default `" = "`
#'
#' @param old `<chr>` Old prefix
#'
#' @param collapse `<chr>` Separator between sequences, default `", "`
#'
#' @param enclose `<chr>` *(optional)* Vector of `length(x) == 2` with which to enclose output
#'
#' @param style `<lgl>` Apply `styler::style_text()` to output, default `TRUE`
#'
#' @returns `<chr>` collapsed vector of `n` sequences
#'
#' @examples
#' rename_seq(
#' n        = 10,
#' new      = "id_issuer_",
#' between  = " = ",
#' old      = "Other.ID.Issuer.",
#' enclose  = c("x = c(", ")"),
#' collapse = ",\n ",
#' style    = TRUE)
#'
#' @autoglobal
#'
#' @export
rename_seq <- function(n,
                       new,
                       between = " = ",
                       old,
                       collapse = ", ",
                       enclose = NULL,
                       style = TRUE) {

  x <- stringr::str_c(
    new,
    seq(n),
    between,
    old,
    seq(n),
    collapse = collapse)

  if (!is.null(enclose)) {
    x <- stringr::str_c(
      enclose[1],
      x,
      enclose[2]
    )
  }

  if (style) {
    x <- styler::style_text(x)
  }
  return(x)
}

#' Format multiple line character vector to single line
#'
#' @param x `<chr>` character vector with line breaks (`\n`)
#'
#' @returns `<chr>` single line character vector
#'
#' @examples
#' single_line_string(
#' "(Jan(?:uary)?|Feb(?:ruary)?|Mar(?:ch)?|
#' Apr(?:il)?|May|Jun(?:e)?|Jul(?:y)?|")
#'
#' @autoglobal
#'
#' @export
single_line_string <- function(x) {

  stringr::str_remove_all(
    x,
    r"(\n\s*)"
  )

}

#' Wrapper for [paste0()] that collapses result
#'
#' @param x A split `<chr>` vector
#'
#' @returns A collapsed `<chr>` string
#'
#' @examples
#' collapser(c("X", "Y", "Z"))
#'
#' @autoglobal
#'
#' @export
collapser <- function(x) {
  paste0(x, collapse = "")
}

#' Wrapper for [unlist()], with `use.names` set to `FALSE`
#'
#' @param x A named `<list>`
#'
#' @returns An unnamed `<chr>` vector
#'
#' @examples
#' delister(list(x = "XYZ"))
#'
#' @autoglobal
#'
#' @export
delister <- function(x) {
  unlist(
    x,
    use.names = FALSE)
}

#' Wrapper for [strsplit()] that unlists and unnames results
#'
#' @param x `<chr>` string or named `<list>` of `<chr>` strings
#'
#' @returns An unnamed `<list>` of split `<chr>` vectors
#'
#' @examples
#' # unnamed vector
#' splitter("XYZ")
#'
#' # named vector
#' splitter(c(x = "XYZ"))
#'
#' # unnamed list with one element
#' splitter(list("XYZ"))
#'
#' # unnamed list with multiple elements
#' splitter(list("YYY", "ZZZ"))
#'
#' # named list with one element
#' splitter(list(x = "XYZ"))
#'
#' # named list with multiple elements
#' splitter(list(x = "YYY", xx = "ZZZ"))
#'
#' @autoglobal
#'
#' @export
splitter <- function(x) {

  res <- strsplit(delister(x), "")

  if (length(res) == 1) {
    return(res[[1]])
  } else {
    return(res)
  }
}
