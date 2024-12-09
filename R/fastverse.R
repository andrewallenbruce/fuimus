#' If Else wrapper using [kit::iif()]
#'
#' @param x `<logical>` vector
#'
#' @param yes,no Values to return depending on TRUE/FALSE element of `x`. Must
#'   be same type and be either length 1 or same length of `x`.
#'
#' @returns vector of same length as `x` and attributes as `yes`. Data values
#'          are taken from values of `yes` and `no`.
#'
#' @examples
#' x <- c(1:4, 3:2, 1:4)
#'
#' iif_else(x > 2L, x, x - 1L)
#'
#' @autoglobal
#'
#' @export
iif_else <- \(x, yes, no) kit::iif(test = x, yes = yes, no = no, nThread = 4L)

#' Parallel Sort wrapper using [kit::psort()]
#'
#' @param x `<character>` vector. If other, will default to [base::sort()]
#'
#' @returns `x` in sorted order
#'
#' @examples
#' strsort(random_npi_generator(10))
#'
#' @autoglobal
#'
#' @export
strsort <- \(x) kit::psort(x, nThread = 4L)

#' Predicate to filter out NAs
#'
#' @param x vector
#'
#' @returns `<logical>` vector
#'
#' @examples
#' na(c(NA, "AA"))
#'
#' @autoglobal
#'
#' @export
na <- \(x) cheapr::is_na(x)

#' Predicate to filter out NAs
#'
#' @param x vector
#'
#' @returns `<logical>` vector
#'
#' @examples
#' not_na(c(NA, "AA"))
#'
#' @autoglobal
#'
#' @export
not_na <- \(x) !na(x)

#' Get named element from list
#'
#' @param l named `<list>`
#'
#' @param nm `<character>` element name; can be a regex pattern
#'
#' @returns named `<list>` element
#'
#' @examples
#' list(x1 = NA, x2 = "AA") |> getelem("x2")
#'
#' list(x1 = NA, x2 = "AA") |> getelem("2$")
#'
#' @autoglobal
#'
#' @export
getelem <- \(l, nm) collapse::get_elem(l = l, elem = nm, regex = TRUE)

#' Lengths of Vector
#'
#' @param x vector
#'
#' @returns `<integer>` vector
#'
#' @examples
#' random_npi_generator(10) |> vlen()
#'
#' @autoglobal
#'
#' @export
vlen <- \(x) collapse::vlengths(x, use.names = FALSE)

#' Unique Values of Vector
#'
#' @param x vector
#'
#' @returns vector
#'
#' @examples
#' random_npi_generator(10) |> uniq()
#'
#' @autoglobal
#'
#' @export
uniq <- \(x) collapse::funique(x)

#' Unique Lengths of Vector
#'
#' @param x vector
#'
#' @returns `<integer>` vector
#'
#' @examples
#' random_npi_generator(10) |> uniq_vlen()
#'
#' @autoglobal
#'
#' @export
uniq_vlen <- \(x) uniq(vlen(x))

#' Unique Values with NAs Removed
#'
#' @param x vector
#'
#' @returns vector
#'
#' @examples
#' uniq_narm(c("A", NA, "A", 1))
#'
#' @autoglobal
#'
#' @export
uniq_narm <- \(x) uniq(collapse::na_rm(x))

#' Maximum Vector Length
#'
#' @param x vector
#'
#' @returns `<integer>` vector
#'
#' @examples
#' random_npi_generator(10) |> max_vlen()
#'
#' @autoglobal
#'
#' @export
max_vlen <- \(x) collapse::fmax(vlen(x))
