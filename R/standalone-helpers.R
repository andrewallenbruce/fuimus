# ---
# repo: andrewallenbruce/fuimus
# file: standalone-helpers.R
# last-updated: 2024-12-10
# license: https://unlicense.org
# imports: [kit, cheapr, collapse, stringfish, vctrs, stringi]
# ---
#
# ## Changelog
#
# 2024-12-10:
#
# * Initial version.
#
# nocov start

# vctrs -------------------------------------------------------------------
# @importFrom vctrs vec_is_empty vec_slice vec_in

#' Is Vector Empty?
#'
#' @param x vector
#'
#' @noRd
empty <- \(x) vctrs::vec_is_empty(x)

#' Return NULL if the input is empty, otherwise return the input
#'
#' @param x vector
#'
#' @noRd
null_if_empty <- \(x) if (empty(x)) NULL else x

#' Search in data frame
#'
#' @param data A `<data.frame>` or `<tibble>`
#'
#' @param column column to search; of the form `data[["column"]]` or
#'   `data$column`
#'
#' @param what string or vector to search for in `column`
#'
#' @noRd
search_in_impl <- function(data, column, what) {
  vctrs::vec_slice(
    data,
    vctrs::vec_in(
      column,
      uniq(what)))
}

#' Search in data frame column if search term is not `NULL`
#'
#' @param data `<data.frame>` or `<tibble>`
#'
#' @param column column to search; of the form `data[["column"]]` or
#'   `data$column`
#'
#' @param what string or vector to search for in `column`
#'
#' @noRd
search_in <- function(data, column, what) {

  if (null(what))
    return(data)

  search_in_impl(data, column, what)
}

# kit ---------------------------------------------------------------------
# @importFrom kit iif psort

#' ifelse wrapper using kit::iif
#'
#' @param x `<lgl>` vector
#'
#' @param yes,no Values to return depending on TRUE/FALSE element of `x`. Must
#'   be same type and be either length 1 or same length of `x`.
#'
#' @noRd
iif_else <- \(x, yes, no) kit::iif(test = x, yes = yes, no = no, nThread = 4L)

#' Parallel Sort
#'
#' @param x `<chr>` vector. If other, will default to [base::sort()]
#'
#' @noRd
strsort <- \(x) kit::psort(x, nThread = 4L)

# cheapr -------------------------------------------------------------------
# @importFrom cheapr is_na

#' Predicate to filter out NAs
#'
#' @param x vector
#'
#' @noRd
na <- \(x) cheapr::is_na(x)

#' Predicate to filter out NAs
#'
#' @param x vector
#'
#' @noRd
not_na <- \(x) !na(x)

# collapse -----------------------------------------------------------------
# @importFrom collapse get_elem vlengths funique fmax na_rm

#' Get named element from list
#'
#' @param l named `<list>`
#'
#' @param e `<chr>` element name; can be a regex pattern
#'
#' @noRd
getelem <- \(l, e) collapse::get_elem(l = l, elem = e, regex = TRUE)

#' Lengths of Vector
#'
#' @param x vector
#'
#' @noRd
vlen <- \(x) collapse::vlengths(x, use.names = FALSE)

#' Unique Values of Vector
#'
#' @param x vector
#'
#' @noRd
uniq <- \(x) collapse::funique(x)

#' Unique Lengths of Vector
#'
#' @param x vector
#'
#' @noRd
uniq_vlen <- \(x) uniq(vlen(x))

#' Unique Values with NAs Removed
#'
#' @param x vector
#'
#' @noRd
uniq_narm <- \(x) uniq(collapse::na_rm(x))

#' Maximum Vector Length
#'
#' @param x vector
#'
#' @noRd
max_vlen <- \(x) collapse::fmax(vlen(x))

# stringfish ---------------------------------------------------------------
# @importFrom stringfish sf_substr convert_to_sf sf_nchar sf_grepl sf_gsub sfc sf_collapse sf_split

#' Subset Vector by Range
#'
#' @param x `<chr>` vector
#'
#' @param start `<int>` index start; default is `1`
#'
#' @param stop `<int>` index end
#'
#' @noRd
sf_sub <- \(x, start = 1, stop) stringfish::sf_substr(x, start = start, stop = stop, nthreads = 4L)

#' Convert string to stringfish vector
#'
#' @param x `<chr>` vector
#'
#' @noRd
sf_conv <- \(x) stringfish::convert_to_sf(x)

#' Count number of characters in character vector
#'
#' @param x `<chr>` vector
#'
#' @noRd
sf_chars <- \(x) stringfish::sf_nchar(x, nthreads = 4L)

#' Subset Vector at One Index Point
#'
#' @param x `<chr>` vector
#'
#' @param idx `<int>` index
#'
#' @noRd
sf_at <- \(x, idx = 1) sf_sub(x, start = idx, stop = idx)

#' Detect by Regex
#'
#' @param s `<chr>` vector
#'
#' @param p `<chr>` regex pattern
#'
#' @noRd
sf_detect <- \(s, p) stringfish::sf_grepl(s, p, nthreads = 4L)

#' Detect Opposite by Regex
#'
#' @param s `<chr>` vector
#'
#' @param p `<chr>` regex pattern
#'
#' @noRd
sf_ndetect <- \(s, p) !stringfish::sf_grepl(s, p, nthreads = 4L)

#' Extract by Regex
#'
#' @param s `<chr>` vector
#'
#' @param p `<chr>` regex pattern
#'
#' @noRd
sf_extract <- \(s, p) s[sf_detect(s, p)]

#' Extract Opposite by Regex
#'
#' @param s `<chr>` vector
#'
#' @param p `<chr>` regex pattern
#'
#' @noRd
sf_nextract <- \(s, p) s[sf_ndetect(s, p)]

#' Remove by Regex
#'
#' @param s `<chr>` vector
#'
#' @param p `<chr>` regex pattern
#'
#' @noRd
sf_remove <- \(s, p) stringfish::sf_gsub(s, p, "", nthreads = 4L)

#' Remove single or double quotes from a character string
#'
#' @param x `<chr>` vector to convert
#'
#' @noRd
remove_quotes <- \(x) sf_remove(x, '["\']')

#' Concatenate Vectors
#'
#' @param ... Any number of vectors, coerced to `<chr>` vector, if necessary
#'
#' @noRd
sf_c <- \(...) stringfish::sfc(...)

#' Collapse Vector
#'
#' @param x `<chr>` vector
#'
#' @param sep `<chr>` separator; default is `""`
#'
#' @noRd
sf_smush <- \(x, sep = "") stringfish::sf_collapse(x, collapse = sep)

#' Split String by Delimiter
#'
#' @param x `<chr>` vector
#'
#' @param s `<chr>` delimiter to split by
#'
#' @param fixed `<lgl>` fixed or regex; default is `TRUE`
#'
#' @noRd
sf_strsplit <- \(x, s, fixed = TRUE) stringfish::sf_split(subject = x, split = s, fixed = fixed, nthreads = 4L)

# stringi -----------------------------------------------------------------
# @importFrom stringi stri_rand_strings

#' Generate Random Strings
#'
#' @param n `<int>` number of strings to generate
#'
#' @param ln `<int>` length of each string
#'
#' @param p `<chr>` pattern to sample from; default is `"[A-Z0-9]"`
#'
#' @noRd
random_string <- \(n, ln, p = "[A-Z0-9]") stringi::stri_rand_strings(n = n, length = ln, pattern = p)

# base --------------------------------------------------------------------
#
#' Unlist with no names
#'
#' @param x Named or unnamed `<list>`
#'
#' @returns Unnamed `<chr>` vector
#'
#' @noRd
delist <- \(x) unlist(x, use.names = FALSE)

#' Delist, Unname and Split a String
#'
#' @param x `<chr>` string or named `<list>`
#'
#' @noRd
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
#' @noRd
null <- \(x) is.null(x)

#' Is `x` not `NULL`?
#'
#' @param x input
#'
#' @returns `<lgl>` `TRUE` if `x` is not `NULL`, else `FALSE`
#'
#' @noRd
not_null <- \(x) !null(x)

# nocov end
