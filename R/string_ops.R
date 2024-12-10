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
na_if_common <- \(x) dplyr::case_match(x, c("", " ", "*", "--", "N/A") ~ NA_character_, .default = x)

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

  if (null(digits)) digits <- max(nchar(x), na.rm = TRUE)

  stopifnot(
    rlang::is_scalar_integer(digits),
    digits > 1
  )

  x   <- as.character(x)
  rpt <- digits - nchar(x)
  rpt <- dplyr::if_else(rpt < 0L, 0L, rpt)

  paste0(
    sapply(rpt,
      FUN = function(x)
        paste0(rep("0", times = x), collapse = "")), x)
}

#' Generate code from a vector of values
#'
#' @param x `<vec>` vector
#'
#' @param collapse `<chr>` Separator between sequences, default `", "`
#'
#' @param enclose `<chr>` *(optional)* Vector of `length(x) == 2` with which to enclose output
#'
#' @param style `<lgl>` Apply [styler::style_text()] to output, default `TRUE`
#'
#' @param ... Additional arguments passed to [styler::tidyverse_style()]
#'
#' @returns `<chr>` vector of valid R code
#'
#' @examples
#' hcpcs <- c(
#'    "63091", "81536", "99305", "63012",
#'    "78835", "E0185", "44408", "87485",
#'    "36015", "0581F", "33478", "21184",
#'    "42999", "15155", "76705", "23412",
#'    "99406", "0585T", "0272T", "92507")
#'
#' create_vec(x = hcpcs)
#'
#' @autoglobal
#'
#' @export
create_vec <- function(x,
                       collapse = ", ",
                       enclose = c("x = c(", ")"),
                       style = TRUE,
                       ...) {

  paras <- "'"

  x <- paste0(paras, gsub(" ", "", uniq_narm(x)), paras, collapse = collapse)

  x <- paste0(enclose[1], x, enclose[2], collapse = "")

  if (style) x <- styler::style_text(text = x, ...)

  return(x)
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
#'    n        = 10,
#'    new      = "id_issuer_",
#'    between  = " = ",
#'    old      = "Other.ID.Issuer.",
#'    enclose  = c("x = c(", ")"),
#'    collapse = ",\n ",
#'    style    = TRUE)
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
  n <- seq.int(n)

  x <- paste0(new, n, between, old, n, collapse = collapse)

  if (not_null(enclose)) x <- paste0(enclose[1], x, enclose[2])

  if (style)
    return(styler::style_text(x))

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
#'    "(Jan(?:uary)?|
#'     Feb(?:ruary)?|
#'     Mar(?:ch)?|"
#'  )
#'
#' @autoglobal
#'
#' @export
single_line_string <- function(x) {
  stringr::str_remove_all(x, r"(\n\s*)")
}


#' Wrapper for [paste0()] that adds brackets
#'
#' @param x `<chr>` string
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
bracks <- function(x) {
  paste0(r"--{[}--", x, r"--{]}--")
}


#' Wrapper for [paste0()] that adds parentheses
#'
#' @param x `<chr>` string
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
parens <- function(x) {
  paste0(r"--{(}--", x, r"--{)}--")
}

#' Wrapper for [paste0()] that adds angle brackets
#'
#' @param x `<chr>` string
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
arrows <- function(x) {
  paste0(r"--{<}--", x, r"--{>}--")
}

#' Print a named list
#'
#' @param ls `<list>` to print
#'
#' @param prefix `<chr>` to prepend to each line
#'
#' @returns `<list>` invisibly
#'
#' @examples
#' print_ls(list(a = 1, b = 2, c = 3))
#'
#' @autoglobal
#'
#' @export
print_ls <- function(ls, prefix = "") {

  if (length(ls) == 0) cat("<empty>\n")

  ns <- names(ls)

  if (length(ns) != length(ls)) stop("all elements must be named")

  ls <- lapply(ls, as.character)

  cat(sprintf("%s%s : %s", prefix, format(ns), ls), sep = "\n")

  invisible(ls)
}

#' String interpolation
#'
#' Expressions enclosed by specified delimiters will be evaluated as R code
#' within the context of the \code{src} data/environment.  The results will
#' then be inserted into the original string via \code{sprintf()}
#' i.e. string interpolation.
#'
#'
#' @param fmt single `<character>` string containing the format specification.
#' @param src data source. An \code{environment}, \code{list},
#'        \code{data.frame} or anything supported by \code{as.environment()}.
#'        Default: \code{parent.frame()} i.e. the calling environment
#' @param open,close the opening and closing `<character>` strings which delimit an expression.
#'        Default: \code{{}}.  Note: the delimiters can be more complex than
#'        just a single character
#' @param eval `<logical>`. Should the expressions be treated as R code to be
#'        evaluated? Default: TRUE means to treat the expressions as R code and
#'        evaluate.  If FALSE, then no code evaluation will ever be
#'        done and expressions will be treated as only variable
#'        names in the given \code{src} data.  This may be safer in some contexts
#'        e.g. for user supplied fmt strings.
#'
#' @returns A `<character>` string with the expressions replaced by their values
#'
#' @examples
#' gluestick("Hello {name}", list(name = '#RStats'))
#'
#' gluestick("Hello ~!name!~", list(name = '#RStats'), open = "~!", close = "!~")
#'
#' name <- '#RStats'
#' gluestick("Hello {name}")
#'
#' @autoglobal
#'
#' @export
gluestick <- function(fmt, src = parent.frame(), open = "{", close = "}", eval = TRUE) {

  nchar_open  <- nchar(open)
  nchar_close <- nchar(close)

  # Sanity checks
  stopifnot(exprs = {
    is.character(fmt)
    length(fmt) == 1L
    is.character(open)
    length(open) == 1L
    nchar_open > 0L
    is.character(close)
    length(close) == 1
    nchar_close > 0
  })

  # Brute force the open/close characters into a regular expression for
  # extracting the expressions from the format string
  open  <- gsub("(.)", "\\\\\\1", open ) # Escape everything!!
  close <- gsub("(.)", "\\\\\\1", close) # Escape everything!!
  re    <- paste0(open, ".*?", close)

  # Extract the delimited expressions
  matches  <- gregexpr(re, fmt)
  exprs    <- regmatches(fmt, matches)[[1]]


  # Remove the delimiters
  exprs <- substr(exprs, nchar_open + 1L, nchar(exprs) - nchar_close)


  # create a valid sprintf fmt string.
  #  - replace all "{expr}" strings with "%s"
  #  - escape any '%' so sprintf() doesn't try and use them for formatting
  #    but only if the '%' is NOT followed by an 's'
  # gluestick() doesn't deal with any pathological cases

  fmt_sprintf <- gsub(re      , "%s", fmt)
  fmt_sprintf <- gsub("%(?!s)", "%%", fmt_sprintf, perl=TRUE)


  # Evaluate
  if (eval) {
    args <- lapply(exprs, function(expr) {eval(parse(text = expr), envir = src)})
  } else {
    args <- unname(mget(exprs, envir = as.environment(src)))
  }

  # Create the string(s)
  do.call(sprintf, c(list(fmt_sprintf), args))
}
