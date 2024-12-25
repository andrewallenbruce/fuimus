# ---
# repo: andrewallenbruce/fuimus
# file: standalone-helpers.R
# last-updated: 2024-12-25
# license: https://unlicense.org
# imports: [cheapr (>= 0.9.92), collapse (>= 2.0.18), kit (>= 0.0.19), pins (>= 1.4.0), stringfish (>= 0.16.0), stringi (>= 1.8.4), vctrs (>= 0.6.5)]
# ---
#
# ## Changelog
#
# 2024-12-25:
#
# * Added:
#    * mount_board()
#    * get_pin()
#    * list_pins()
#
# 2024-12-15:
#
# * Added gelm()
# * Fixed errors in roxygen documentation
#
# 2024-12-14:
#
# * Added:
#    * remove_all_na()
#    * na_if()
#
# 2024-12-13:
#
# * Added sf_replace()
#
# 2024-12-12:
#
# * Fixed bug in search_in()
# * Shortened as_() function names
# * Added roundup()
#
# 2024-12-11:
#
# * Renamed null_if_empty() to if_empty_null()
#
# * Added:
#    * invert_named()
#    * true()
#    * false()
#    * as_character()
#    * as_integer()
#    * as_numeric()
#    * as_date()
#
# 2024-12-10:
#
# * Initial version.
#
# nocov start

# pins --------------------------------------------------------------------
#
#' Mount [pins][pins::pins-package] board
#'
#' @param source `<chr>` `"local"` or `"remote"`
#'
#' @returns `<pins_board_folder>` or `<pins_board_url>`
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
mount_board <- \(source = c("local", "remote")) {

  source <- match.arg(source)

  gh_raw  <- \(x) paste0("https://raw.githubusercontent.com/", x)

  gh_path <- gh_raw(
    paste0(
      "andrewallenbruce/",
      utils::packageName(),
      "/master/inst/extdata/pins/"))

  switch(
    source,
    local = pins::board_folder(
      fs::path_package("extdata/pins", package = utils::packageName())),
    remote = pins::board_url(gh_path))
}

#' Get pinned dataset from mount_board()
#'
#' @param pin `<chr>` string name of pinned dataset
#'
#' @param ... additional arguments passed to mount_board()
#'
#' @returns `<tibble>` or `<data.frame>`
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
get_pin <- function(pin, ...) {

  board <- mount_board(...)

  pin <- rlang::arg_match0(pin, list_pins())

  pins::pin_read(board, pin)

}

#' List pins from mount_board()
#'
#' @param ... arguments to pass to mount_board()
#'
#' @returns `<chr>` vector of named pins
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
list_pins <- function(...) {

  board <- mount_board(...)

  pins::pin_list(board)

}

# vctrs -------------------------------------------------------------------
#
#' Is Vector Empty?
#'
#' @param x vector
#'
#' @returns `<lgl>` `TRUE` if empty, else `FALSE`
#'
#' @keywords internal
#'
#' @autoglobal
#'
#' @export
empty <- \(x) { vctrs::vec_is_empty(x) }

#' If `x` is empty, `NULL`, else `x`
#'
#' @param x vector
#'
#' @returns `x` or `NULL`
#'
#' @keywords internal
#'
#' @autoglobal
#'
#' @export
if_empty_null <- \(x) { if (empty(x)) NULL else x }

#' Search in data frame
#'
#' @param x vector or `<data.frame>`
#'
#' @param column `<chr>` string of column name
#'
#' @param what to search for in `column`
#'
#' @noRd
search_in_impl <- \(x, column, what) vctrs::vec_slice(x, vctrs::vec_in(x[[column]], uniq(what)))

#' Search in data frame column if search term is not `NULL`
#'
#' @param x vector or `<data.frame>`
#'
#' @param column `<chr>` string of column name
#'
#' @param what to search for in `column`
#'
#' @returns vector or `<data.frame>`
#'
#' @keywords internal
#'
#' @autoglobal
#'
#' @export
search_in <- \(x, column, what) {

  if (null(what)) return(x)

  search_in_impl(x, column, what)
}

#' Convert values to `NA`
#'
#' @param x vector to modify
#'
#' @param y values to convert to `NA`. Type must match that of `x`.
#'
#' @returns `x` with values in `y` converted to `NA`
#'
#' @keywords internal
#'
#' @autoglobal
#'
#' @export
na_if <- \(x, y) {

  vctrs::vec_slice(
    x,
    vctrs::vec_in(
      x,
      y,
      needles_arg = "x",
      haystack_arg = "y")
    ) <- NA
  x
}

# kit ---------------------------------------------------------------------
#
#' ifelse wrapper using kit::iif
#'
#' @param x `<lgl>` vector
#'
#' @param yes,no Values to return depending on TRUE/FALSE element of `x`. Must
#'   be same type and be either length 1 or same length of `x`.
#'
#' @returns `<chr>` vector
#'
#' @keywords internal
#'
#' @autoglobal
#'
#' @export
iif_else <- \(x, yes, no) { kit::iif(test = x, yes = yes, no = no, nThread = 4L) }

#' Parallel Sort
#'
#' @param x `<chr>` vector. If other, will default to [base::sort()]
#'
#' @returns `<chr>` vector
#'
#' @keywords internal
#'
#' @autoglobal
#'
#' @export
strsort <- \(x) { kit::psort(x, nThread = 4L) }

# cheapr -------------------------------------------------------------------
#
#' Predicate to filter out NAs
#'
#' @param x vector
#'
#' @returns `<lgl>` vector
#'
#' @keywords internal
#'
#' @autoglobal
#'
#' @export
na <- \(x) { cheapr::is_na(x) }

#' Predicate to filter out NAs
#'
#' @param x vector
#'
#' @returns `<lgl>` vector
#'
#' @keywords internal
#'
#' @autoglobal
#'
#' @export
not_na <- \(x) { !na(x) }

#' Remove columns and rows with all NAs
#'
#' @param x vector or `<data.frame>`
#'
#' @returns vector or `<data.frame>` with columns and rows with all NAs removed
#'
#' @keywords internal
#'
#' @autoglobal
#'
#' @export
remove_all_na <- \(x) { cheapr::na_rm(x[, !cheapr::col_all_na(x)]) }

# collapse -----------------------------------------------------------------
#
#' Get named element from list
#'
#' @param l named `<list>`
#'
#' @param e `<chr>` element name; can be a regex pattern
#'
#' @returns `<chr>` element value
#'
#' @keywords internal
#'
#' @autoglobal
#'
#' @export
getelem <- \(l, e) { collapse::get_elem(l = l, elem = e, regex = TRUE) }

#' `getelem` with more flexibility
#'
#' @param l named `<list>`
#'
#' @param e `<chr>` element name; can be a regex pattern if `m` is `"re"`
#'
#' @param m `<chr>` mode; default is `"re"` (regex) or `"df"` (data frame)
#'
#' @param ... additional arguments to pass to `collapse::get_elem()`
#'
#' @returns `<chr>` or `<data.frame>`, depending on `m`
#'
#' @keywords internal
#'
#' @autoglobal
#'
#' @export
gelm <- \(l, e, m = "re", ...) {

  collapse::get_elem(
    l = l,
    elem = e,
    regex = ifelse(m == "re", TRUE, FALSE),
    DF.as.list = ifelse(m == "df", TRUE, FALSE), ...)

}

#' Lengths of Vector
#'
#' @param x vector
#'
#' @returns `<int>` vector
#'
#' @keywords internal
#'
#' @autoglobal
#'
#' @export
vlen <- \(x) { collapse::vlengths(x, use.names = FALSE) }

#' Unique Values of Vector
#'
#' @param x vector
#'
#' @returns `<chr>` vector
#'
#' @keywords internal
#'
#' @autoglobal
#'
#' @export
uniq <- \(x) { collapse::funique(x) }

#' Unique Lengths of Vector
#'
#' @param x vector
#'
#' @returns `<int>` vector
#'
#' @keywords internal
#'
#' @autoglobal
#'
#' @export
uniq_vlen <- \(x) { uniq(vlen(x)) }

#' Unique Values with NAs Removed
#'
#' @param x vector
#'
#' @returns `<chr>` vector
#'
#' @keywords internal
#'
#' @autoglobal
#'
#' @export
uniq_narm <- \(x) { uniq(collapse::na_rm(x)) }

#' Maximum Vector Length
#'
#' @param x vector
#'
#' @returns `<int>` maximum length of vector
#'
#' @keywords internal
#'
#' @autoglobal
#'
#' @export
max_vlen <- \(x) { collapse::fmax(vlen(x)) }

# stringfish ---------------------------------------------------------------
#
#' Subset Vector by Range
#'
#' @param x `<chr>` vector
#'
#' @param start `<int>` index start; default is `1`
#'
#' @param stop `<int>` index end
#'
#' @returns `<chr>` vector
#'
#' @keywords internal
#'
#' @autoglobal
#'
#' @export
sf_sub <- \(x, start = 1, stop) { stringfish::sf_substr(x, start = start, stop = stop, nthreads = 4L) }

#' Convert string to stringfish vector
#'
#' @param x `<chr>` vector
#'
#' @returns `<chr>` vector
#'
#' @keywords internal
#'
#' @autoglobal
#'
#' @export
sf_conv <- \(x) { stringfish::convert_to_sf(x) }

#' Count number of characters in character vector
#'
#' @param x `<chr>` vector
#'
#' @returns `<int>` vector
#'
#' @keywords internal
#'
#' @autoglobal
#'
#' @export
sf_chars <- \(x) { stringfish::sf_nchar(x, nthreads = 4L) }

#' Subset Vector at One Index Point
#'
#' @param x `<chr>` vector
#'
#' @param idx `<int>` index
#'
#' @returns `<chr>` vector
#'
#' @keywords internal
#'
#' @autoglobal
#'
#' @export
sf_at <- \(x, idx = 1) { sf_sub(x, start = idx, stop = idx) }

#' Detect by Regex
#'
#' @param s `<chr>` vector
#'
#' @param p `<chr>` regex pattern
#'
#' @returns `<lgl>` vector
#'
#' @keywords internal
#'
#' @autoglobal
#'
#' @export
sf_detect <- \(s, p) { stringfish::sf_grepl(s, p, nthreads = 4L) }

#' Detect Opposite by Regex
#'
#' @param s `<chr>` vector
#'
#' @param p `<chr>` regex pattern
#'
#' @returns `<lgl>` vector
#'
#' @keywords internal
#'
#' @autoglobal
#'
#' @export
sf_ndetect <- \(s, p) { !stringfish::sf_grepl(s, p, nthreads = 4L) }

#' Extract by Regex
#'
#' @param s `<chr>` vector
#'
#' @param p `<chr>` regex pattern
#'
#' @returns `<chr>` vector with pattern extracted
#'
#' @keywords internal
#'
#' @autoglobal
#'
#' @export
sf_extract <- \(s, p) { s[sf_detect(s, p)] }

#' Extract Opposite by Regex
#'
#' @param s `<chr>` vector
#'
#' @param p `<chr>` regex pattern
#'
#' @returns `<chr>` vector with pattern extracted
#'
#' @keywords internal
#'
#' @autoglobal
#'
#' @export
sf_nextract <- \(s, p) { s[sf_ndetect(s, p)] }

#' Replace by Regex
#'
#' @param s `<chr>` vector
#'
#' @param p `<chr>` regex pattern
#'
#' @param r `<chr>` replacement
#'
#' @param fix `<lgl>` fixed or regex; default is `FALSE`
#'
#' @returns `<chr>` vector with pattern replaced
#'
#' @keywords internal
#'
#' @autoglobal
#'
#' @export
sf_replace <- \(s, p, r, fix = FALSE) { stringfish::sf_gsub(subject = s, pattern = p, replacement = r, fixed = fix, nthreads = 4L) }

#' Remove by Regex
#'
#' @param s `<chr>` vector
#'
#' @param p `<chr>` regex pattern
#'
#' @param fix `<lgl>` fixed or regex; default is `FALSE`
#'
#' @returns `<chr>` vector with pattern removed
#'
#' @keywords internal
#'
#' @autoglobal
#'
#' @export
sf_remove <- \(s, p, fix = FALSE) { stringfish::sf_gsub(subject = s, pattern = p, replacement = "", fixed = fix, nthreads = 4L) }

#' Remove single or double quotes from a character string
#'
#' @param x `<chr>` vector to convert
#'
#' @returns `<chr>` vector with quotes removed
#'
#' @keywords internal
#'
#' @autoglobal
#'
#' @export
remove_quotes <- \(x) { if (is.character(x)) sf_remove(x, '["\']') else x }

#' Concatenate Vectors
#'
#' @param ... Any number of vectors, coerced to `<chr>` vector, if necessary
#'
#' @returns `<chr>` concatenated vector
#'
#' @keywords internal
#'
#' @autoglobal
#'
#' @export
sf_c <- \(...) { stringfish::sfc(...) }

#' Collapse Vector
#'
#' @param x `<chr>` vector
#'
#' @param sep `<chr>` separator; default is `""`
#'
#' @returns `<chr>` collapsed vector
#'
#' @keywords internal
#'
#' @autoglobal
#'
#' @export
sf_smush <- \(x, sep = "") { stringfish::sf_collapse(x, collapse = sep) }

#' Split string by delimiter
#'
#' @param x `<chr>` vector
#'
#' @param s `<chr>` delimiter to split by
#'
#' @param fix `<lgl>` fixed or regex; default is `TRUE`
#'
#' @returns `<list>` of split strings
#'
#' @keywords internal
#'
#' @autoglobal
#'
#' @export
sf_strsplit <- \(x, s, fix = TRUE) { stringfish::sf_split(subject = x, split = s, fixed = fix, nthreads = 4L) }

# stringi -----------------------------------------------------------------
#
#' Generate random strings
#'
#' @param n `<int>` number of strings to generate
#'
#' @param ln `<int>` length of each string
#'
#' @param p `<chr>` pattern to sample from; default is `"[A-Z0-9]"`
#'
#' @returns `<chr>` vector of random strings
#'
#' @keywords internal
#'
#' @autoglobal
#'
#' @export
random_string <- \(n, ln, p = "[A-Z0-9]") { stringi::stri_rand_strings(n = n, length = ln, pattern = p) }

# base --------------------------------------------------------------------
#
#' Unlist and unname
#'
#' @param x Named or unnamed `<list>`
#'
#' @returns Unnamed `<chr>` vector
#'
#' @keywords internal
#'
#' @autoglobal
#'
#' @export
delist <- \(x) { unlist(x, use.names = FALSE) }

#' Unlist, unname and split
#'
#' @param x `<chr>` string or named `<list>`
#'
#' @keywords internal
#'
#' @autoglobal
#'
#' @export
desplit <- \(x) {

  res <- sf_strsplit(delist(x), "")

  if (length(res) == 1) return(res[[1]])

  res
}

#' Is `x` `NULL`?
#'
#' @param x input
#'
#' @returns `<lgl>` `TRUE` if `x` is `NULL`, else `FALSE`
#'
#' @keywords internal
#'
#' @autoglobal
#'
#' @export
null <- \(x) { is.null(x) }

#' Is `x` not `NULL`?
#'
#' @param x input
#'
#' @returns `<lgl>` `TRUE` if `x` is not `NULL`, else `FALSE`
#'
#' @keywords internal
#'
#' @autoglobal
#'
#' @export
not_null <- \(x) { !null(x) }

#' Is `x` `TRUE`?
#'
#' @param x input
#'
#' @returns `<lgl>` `TRUE` if `x` is `TRUE`, else `FALSE`
#'
#' @keywords internal
#'
#' @autoglobal
#'
#' @export
true <- \(x) { isTRUE(x) }

#' Is `x` `FALSE`?
#'
#' @param x input
#'
#' @returns `<lgl>` `TRUE` if `x` is `FALSE`, else `FALSE`
#'
#' @keywords internal
#'
#' @autoglobal
#'
#' @export
false <- \(x) { isFALSE(x) }

#' Coerce vector to `<chr>`
#'
#' @param x vector
#'
#' @returns `<chr>` vector
#'
#' @keywords internal
#'
#' @autoglobal
#'
#' @export
as_chr <- \(x) { if (is.character(x)) x else as.character(x) }

#' Coerce vector to `<int>`
#'
#' @param x vector
#'
#' @returns `<int>` vector
#'
#' @keywords internal
#'
#' @autoglobal
#'
#' @export
as_int <- \(x) { if (is.integer(x)) x else as.integer(x) }

#' Coerce vector to `<num>` class
#'
#' @param x vector
#'
#' @returns `<num>` vector
#'
#' @keywords internal
#'
#' @autoglobal
#'
#' @export
as_num <- \(x) { if (is.numeric(x)) x else as.numeric(x) }

#' Coerce vector to `<date>` class
#'
#' @param x vector
#'
#' @param ... additional arguments
#'
#' @param fmt `<chr>` format; default is `"%Y-%m-%d"`
#'
#' @returns `<date>` vector
#'
#' @keywords internal
#'
#' @autoglobal
#'
#' @export
as_date <- \(x, ..., fmt = "%Y-%m-%d") { as.Date(x, ..., format = fmt) }

#' Invert a named vector
#'
#' @param x A named vector
#'
#' @returns A named vector with names and values inverted
#'
#' @keywords internal
#'
#' @autoglobal
#'
#' @export
invert_named <- \(x) {

  stopifnot("Input must be a named vector" = not_null(names(x)))

  stats::setNames(names(x), unname(x))
}

#' Round up to nearest decimal place
#'
#' @param x `<num>` vector
#'
#' @param d `<int>` decimal places to round to; default is `2`
#'
#' @returns `<num>` vector rounded up to the nearest decimal place
#'
#' @keywords internal
#'
#' @autoglobal
#'
#' @export
roundup <- \(x, d = 2) {

  d  <- 10^d

  z  <- abs(x) * d

  z  <- z + 0.5 + sqrt(.Machine[["double.eps"]])

  z  <- trunc(z)

  z  <- z / d

  z * sign(x)
}
# nocov end
