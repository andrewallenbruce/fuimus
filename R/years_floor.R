#' Calculate the number of years between two dates rounded down
#'
#' @param from `<date>` Start date
#'
#' @param to `<date>` End date
#'
#' @returns `<dbl>` vector of the number of years between the two dates
#'
#' @examples
#' years_floor(
#'    from = as.Date("2020-01-01"),
#'    to   = as.Date("2020-01-01") + 2057)
#'
#' @autoglobal
#'
#' @export
years_floor <- function(from, to) {
  floor(
    as.integer(
      difftime(
        to,
        from,
        units = "weeks",
        tz = "UTC"
      )
    ) / 52.17857)
}
