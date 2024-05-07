#' Calculate Number of Days Between Two Dates
#'
#' @note This calculation includes the end date in the sum (see example)
#'
#' @param df `<data.frame>` containing date columns
#'
#' @param start column containing date(s) prior to end_date column
#'
#' @param end column containing date(s) after start_date column
#'
#' @param colname desired column name of output; default is "age"
#'
#' @returns A [tibble][tibble::tibble-package] with a named column
#'    containing the calculated number of days.
#'
#' @examples
#' date_ex <- dplyr::tibble(
#'    x = seq.Date(
#'    as.Date("2021-01-01"),
#'    by = "month",
#'    length.out = 3),
#'    y = seq.Date(
#'    as.Date("2022-01-01"),
#'    by = "month",
#'    length.out = 3
#'    )
#'  )
#'
#' age_days(df    = date_ex,
#'          start = x,
#'          end   = y)
#'
#' date_ex |>
#' age_days(x, y, "days_between_x_y")
#'
#' date_ex |>
#' age_days(start = x,
#' end = lubridate::today(),
#' colname = "days_since_x")
#'
#' date_ex |>
#' age_days(x, y, "days_between_x_y") |>
#' age_days(x, lubridate::today(), "days_since_x") |>
#' age_days(y, lubridate::today(), colname = "days_since_y")
#'
#' @autoglobal
#'
#' @export
age_days <- function(df,
                     start,
                     end,
                     colname = "age") {

  stopifnot(inherits(df, "data.frame"))

  df |>
    dplyr::mutate(
      start = as.Date(
        {{ start }},
        "%yyyy-%mm-%dd",
        tz = "EST"
      ),
      end = as.Date(
        {{ end }},
        "%yyyy-%mm-%dd",
        tz = "EST"
      )
    ) |>
    dplyr::mutate(
      "{colname}" := ((
        as.numeric(
          lubridate::days(end) - lubridate::days(start), "hours") / 24) + 1
      )
    ) |>
    dplyr::select(
      !c(
        end,
        start
      )
    )
}

#' Count days between two dates
#'
#' @param df A `<data.frame>` or `<tibble>`
#'
#' @param start `<sym>` bare date column name
#'
#' @param end `<sym>` bare date column name
#'
#' @param name `<sym>` bare name of days output column
#'
#' @returns `<data.frame>` or `<tibble>`
#'
#' @autoglobal
#'
#' @export
#'
#' @examples
#' dplyr::tibble(dos    = as.Date(c("2021-04-18", "2021-11-18", "2022-02-18")),
#'               signed = as.Date("2022-02-18")) |>
#'               count_days(
#'               start  = dos,
#'               end    = signed,
#'               lag)
#'
count_days <- function(df,
                       start,
                       end,
                       name) {
  df |>
    dplyr::mutate(
      {{ name }} := clock::date_count_between(
        {{ start }},
        {{ end }},
        "day"),
      .after = {{ end }}
    )
}

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
