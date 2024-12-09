#' Subset Vector by Range
#'
#' @param x `<character>` vector
#'
#' @param start `<integer>` index start; default is `1`
#'
#' @param stop `<integer>` index end
#'
#' @returns `<character>` vector
#'
#' @examples
#' sf_sub(random_npi_generator(10), 1, 2)
#'
#' @autoglobal
#'
#' @export
sf_sub <- \(x, start = 1, stop) stringfish::sf_substr(x, start = start, stop = stop, nthreads = 4L)

#' Convert string to stringfish vector
#'
#' @param x `<character>` vector
#'
#' @returns `<character>` stringfish vector
#'
#' @examples
#' sf_convert(random_npi_generator(10))
#'
#' @autoglobal
#'
#' @export
sf_convert <- \(x) stringfish::convert_to_sf(x)

#' Count number of characters in character vector
#'
#' @param x `<character>` vector
#'
#' @returns `<character>` stringfish vector
#'
#' @examples
#' sf_nchar(random_npi_generator(10))
#'
#' @autoglobal
#'
#' @export
sf_nchar <- \(x) stringfish::sf_nchar(x, nthreads = 4L)

#' Subset Vector at Index
#'
#' @param x `<character>` vector
#'
#' @param start `<integer>` index
#'
#' @returns `<character>` vector
#'
#' @examples
#' take_at(random_npi_generator(10), 2)
#'
#' @autoglobal
#'
#' @export
take_at <- \(x, start = 1) sf_sub(x, start = start, stop = start)

#' Detect by Regex
#'
#' @param s `<character>` vector
#'
#' @param p `<character>` regex pattern
#'
#' @returns `<logical>` vector
#'
#' @examples
#' sf_detect(random_npi_generator(10), "[A-Z]{1}")
#'
#' @autoglobal
#'
#' @export
sf_detect <- \(s, p) stringfish::sf_grepl(s, p, nthreads = 4L)

#' Detect Opposite by Regex
#'
#' @param s `<character>` vector
#'
#' @param p `<character>` regex pattern
#'
#' @returns `<logical>` vector
#'
#' @examples
#' sf_ndetect(random_npi_generator(10), "[A-Z]{1}")
#'
#' @autoglobal
#'
#' @export
sf_ndetect <- \(s, p) !stringfish::sf_grepl(s, p, nthreads = 4L)

#' Extract by Regex
#'
#' @param s `<character>` vector
#'
#' @param p `<character>` regex pattern
#'
#' @returns `<character>` vector
#'
#' @examples
#' sf_extract(sf_c(LETTERS, 0:9), "[A-Z]")
#'
#' sf_extract(random_npi_generator(10), "[123]")
#'
#' @autoglobal
#'
#' @export
sf_extract <- \(s, p) s[sf_detect(s, p)]

#' Extract Opposite by Regex
#'
#' @param s `<character>` vector
#'
#' @param p `<character>` regex pattern
#'
#' @returns `<character>` vector
#'
#' @examples
#' sf_nextract(sf_c(LETTERS, 0:9), "[A-Z]")
#'
#' sf_nextract(random_npi_generator(10), "[A-Z]")
#'
#' @autoglobal
#'
#' @export
sf_nextract <- \(s, p) s[sf_ndetect(s, p)]

#' Remove by Regex
#'
#' @param s `<character>` vector
#'
#' @param p `<character>` regex pattern
#'
#' @returns `<character>` vector
#'
#' @examples
#' sf_remove(LETTERS, "A")
#'
#' sf_remove(paste0(LETTERS, collapse = ""), "A")
#'
#' @autoglobal
#'
#' @export
sf_remove <- \(s, p) stringfish::sf_gsub(s, p, "", nthreads = 4L)

#' Concatenate Vectors
#'
#' @param ... Any number of vectors, coerced to `<character>` vector, if necessary
#'
#' @returns concatenated `<character>` vector
#'
#' @examples
#' sf_c(LETTERS, "A")
#'
#' @autoglobal
#'
#' @export
sf_c <- \(...) stringfish::sfc(...)

#' Collapse Vector
#'
#' @param x `<character>` vector
#'
#' @param sep `<character>` separator; default is `""`
#'
#' @returns collapsed `<character>` vector
#'
#' @examples
#' sf_smush(LETTERS, "|")
#'
#' @autoglobal
#'
#' @export
sf_smush <- \(x, sep = "") stringfish::sf_collapse(x, collapse = sep)

#' Split String by Delimiter
#'
#' @param x `<character>` vector
#'
#' @param s `<character>` delimiter to split by
#'
#' @param fixed `<logical>` fixed or regex; default is `TRUE`
#'
#' @returns `<list>` of character vectors
#'
#' @examples
#' sf_strsplit("A|B|C", "|")
#'
#' @autoglobal
#'
#' @export
sf_strsplit <- \(x, s, fixed = TRUE) stringfish::sf_split(subject = x, split = s, fixed = fixed, nthreads = 4L)
