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

#' Calculate the number of years between two dates rounded down to the nearest
#' whole number
#'
#' @param from `<date>` Start date
#'
#' @param to `<date>` End date
#'
#' @returns `<dbl>` vector of the number of years between the two dates rounded
#'   down to the nearest whole number
#'
#' @examples
#' years_floor(
#'    from = as.Date("2020-01-01"),
#'    to = as.Date("2020-01-01") + 2057)
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

#' Calculate the number of years between two dates
#'
#' @param df A `<data.frame>` or `<tibble>`
#'
#' @param from `<date>` Start date column
#'
#' @param to `<date>` End date column
#'
#' @returns `<data.frame>` or `<tibble>`
#'
#' @examples
#' dplyr::tibble(
#'   from = as.Date("2020-01-01"),
#'   to = as.Date("2020-01-01") + 2057) |>
#'   years_df(from, to)
#'
#' @autoglobal
#'
#' @export
years_df <- function(df, from, to) {

  df |>
    dplyr::mutate(
      years_passed = round(
        as.double(
          difftime(
            {{ to }},
            {{ from }},
            units = "weeks",
            tz = "UTC")) / 52.17857, 2),
      .after = {{ to }})
}

#' Calculate the number of years between two dates
#'
#' @param from `<date>` Start date
#'
#' @param to `<date>` End date
#'
#' @returns `<dbl>` vector of the number of years between the two dates
#'
#' @examples
#' years_vec(
#'   from = as.Date("2020-01-01"),
#'   to = as.Date("2020-01-01") + 2057)
#'
#' @autoglobal
#'
#' @export
years_vec <- function(from, to) {

  round(
    as.double(
      difftime(
        {{ to }},
        {{ from }},
        units = "weeks",
        tz = "UTC")) / 52.17857, 2)
}

#' Calculate the duration between two dates
#'
#' @param from `<date>` Start date
#'
#' @param to `<date>` End date
#'
#' @returns `<dbl>` vector of the duration between the two dates
#'
#' @examples
#' duration_vec(
#'   from = lubridate::today() - 366,
#'   to = lubridate::today())
#'
#' @autoglobal
#'
#' @export
duration_vec <- function(from, to) {

  lubridate::as.duration(
    difftime(
      to,
      from,
      units = "auto",
      tz = "UTC"
      )
    )
}

#' Create interval, period and time length columns from a start and end date
#'
#' @param df A `<data.frame>` or `<tibble>`
#'
#' @param start `<date>` Start date column
#'
#' @param end `<date>` End date column
#'
#' @returns `<data.frame>` or `<tibble>`
#'
#' @examples
#' dplyr::tibble(date = lubridate::today() - 366) |>
#'   make_interval(start = date)
#'
#' @autoglobal
#'
#' @export
make_interval <- function(df, start, end = lubridate::today()) {

  df |>
    dplyr::mutate(
      interval = lubridate::interval(
        lubridate::ymd({{ start }}),
        lubridate::ymd({{ end }})),
      period = lubridate::as.period(interval),
      timelength_days = lubridate::time_length(interval, unit = "days"))
}
