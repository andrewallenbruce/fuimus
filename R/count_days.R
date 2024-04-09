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
