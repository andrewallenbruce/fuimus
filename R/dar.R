#' Calculate Average Days in AR
#'
#' @param df `<data.frame>` containing at least three columns: a date column, a
#'   gross charges column and an ending AR column
#'
#' @param date column containing a date within the month that Days in AR is to
#'   be calculated
#'
#' @param gct column containing a month's total Gross Charges
#'
#' @param earb column containing a month's Ending AR balance
#'
#' @param dart Days in AR target, default is `35` days
#'
#' @param period `<chr>` string specifying the period; either "month" or
#'   "quarter"
#'
#' @returns a [tibble][tibble::tibble-package]
#'
#' @examples
#' avg_dar(df     = dar_example(),
#'         date   = date,
#'         gct    = gct,
#'         earb   = earb,
#'         dart   = 35,
#'         period = "month")
#'
#' avg_dar(df     = dar_example(),
#'         date   = date,
#'         gct    = gct,
#'         earb   = earb,
#'         dart   = 35,
#'         period = "quarter")
#'
#' @autoglobal
#'
#' @export
avg_dar <- function(df,
                    date,
                    gct,
                    earb,
                    dart = 35,
                    period = c("month", "quarter")) {

  period <- match.arg(period)

  df <- dplyr::mutate(
    df,
    date  = clock::as_date({{ date }}),
    nmon  = lubridate::month({{ date }}, label = FALSE),
    month = lubridate::month(date, label = TRUE, abbr = FALSE),
    nqtr  = lubridate::quarter({{ date }}),
    ndip  = lubridate::days_in_month({{ date }})
  )

  if (period == "quarter") {

    gctcol <- rlang::englue("{{ gct }}")

    earb_sub <- df |>
      dplyr::filter(nmon %in% c(3, 6, 9, 12)) |>
      dplyr::select(date, earb, nmon, nqtr, month)

    gct_sub <- df |>
      dplyr::summarise(
        "{gctcol}" := roundup(sum({{ gct }})),
        ndip = sum(ndip),
        .by = nqtr)

    df <- dplyr::left_join(
      earb_sub,
      gct_sub,
      by = dplyr::join_by(nqtr)
    )
  }
  df |>
    dplyr::mutate(
      # Average Daily Charge
      adc      = {{ gct }} / ndip,
      # Days in Accounts Receivable
      dar      = {{ earb }} / adc,
      # Actual Ratio of Ending AR to Gross Charges
      actual   = {{ earb }} / {{ gct }},
      # Ideal Ratio of Ending AR to Gross Charges
      ideal    = {{ dart }} / ndip,
      # Actual - Ideal Ratio
      radiff   = actual - ideal,
      # Ending AR Target
      earb_trg = ({{ gct }} * {{ dart }}) / ndip,
      # Ending AR Decrease Needed
      earb_dc  = {{ earb }} - earb_trg,
      # Ending AR Percentage Decrease Needed
      earb_pct = (earb_dc / {{ earb }}) * 100,
      # Boolean indicating whether DAR was under/over DARt
      pass     = dplyr::case_when(dar < {{ dart }} ~ TRUE, TRUE ~ FALSE)) |>
    dplyr::select(
      dplyr::any_of(
        c("date",
          "month",
          "nmon",
          "nqtr",
          "ndip",
          "gct",
          "earb",
          "earb_trg",
          "earb_dc",
          "earb_pct",
          "adc",
          "dar",
          "pass",
          "actual",
          "ideal",
          "radiff")
      )
    )
}

#' Calculate Monthly Days in AR
#'
#' @param df `<data.frame>` containing at least three columns: a date column, a
#'   gross charges column and an ending AR column
#'
#' @param date column containing a date within the month that Days in AR is to
#'   be calculated
#'
#' @param gct column containing a month's total Gross Charges
#'
#' @param earb column containing a month's Ending AR balance
#'
#' @param dart Days in AR target, default is `35` days
#'
#' @returns a [tibble][tibble::tibble-package]
#'
#' @examples
#' dar_month(dar_example(), date, gct, earb, dart = 40)
#'
#' @autoglobal
#'
#' @export
dar_month <- function(df,
                      date,
                      gct,
                      earb,
                      dart = 35) {

  df |>
    dplyr::mutate(
      date     = clock::as_date({{ date }}),
      nmon     = lubridate::month(date, label = FALSE),
      month    = lubridate::month(date, label = TRUE, abbr = FALSE),
      ndip     = lubridate::days_in_month(date),
      adc      = {{ gct }} / ndip,
      dar      = {{ earb }} / adc,
      actual   = {{ earb }} / {{ gct }},
      ideal    = {{ dart }} / ndip,
      radiff   = actual - ideal,
      earb_trg = ({{ gct }} * {{ dart }}) / ndip,
      earb_dc  = {{ earb }} - earb_trg,
      earb_pct = (earb_dc / {{ earb }}) * 100,
      pass     = dplyr::case_when(
        dar < {{ dart }} ~ TRUE,
        TRUE ~ FALSE)
      ) |>
    dplyr::select(date,
                  month,
                  nmon,
                  ndip,
                  gct,
                  earb,
                  earb_trg,
                  earb_dc,
                  earb_pct,
                  adc,
                  dar,
                  pass,
                  actual,
                  ideal,
                  radiff)

}

#' Calculate Quarterly Days in AR
#'
#' @param df `<data.frame>` containing at least three columns: a date column, a
#'   gross charges column and an ending AR column
#'
#' @param date column containing a date within the month that Days in AR is to
#'   be calculated
#'
#' @param gct column containing a month's total Gross Charges
#'
#' @param earb column containing a month's Ending AR balance
#'
#' @param dart Days in AR target, default is `35` days
#'
#' @returns a [tibble][tibble::tibble-package]
#'
#' @examples
#' dar_quarter(
#'    dar_example(),
#'    date,
#'    gct,
#'    earb,
#'    dart = 40)
#'
#' @autoglobal
#'
#' @export
dar_quarter <- function(df,
                    date,
                    gct,
                    earb,
                    dart = 35) {

  base <- dplyr::mutate(df,
                        date = as.Date({{ date }}, "%yyyy-%mm-%dd", tz = "EST"),
                        nmon = lubridate::month({{ date }}, label = FALSE),
                        nqtr = lubridate::quarter({{ date }}),
                        ndip = lubridate::days_in_month({{ date }}))

  earb_sub <- base |>
    dplyr::filter(nmon == 3 |
                  nmon == 6 |
                  nmon == 9 |
                  nmon == 12) |>
    dplyr::select(nqtr, date, {{ earb }})

  gct_sub <- base |>
    dplyr::group_by(nqtr) |>
    dplyr::summarise(gct_qtr = janitor::round_half_up(
      sum(
        {{ gct }}),
      digits = 2
      ),
      ndip = sum(ndip),
      .groups = "drop")

  dplyr::left_join(
    earb_sub,
    gct_sub,
    by = dplyr::join_by(nqtr)
    ) |>
    dplyr::mutate(
    # Average Daily Charge
    adc = janitor::round_half_up(gct_qtr / ndip, digits = 2),

    # Days in Accounts Receivable
    dar = janitor::round_half_up({{ earb }} / adc, digits = 2),

    # Ratio of Ending AR to Gross Charges
    actual = janitor::round_half_up({{ earb }} / gct_qtr, digits = 2),

    # Ideal Ratio of Ending AR to Gross Charges
    ideal = janitor::round_half_up({{ dart }} / ndip, digits = 2),

    # Actual - Ideal Ratio
    radiff = janitor::round_half_up(actual - ideal, digits = 2),

    # Ending AR Target
    earb_trg = janitor::round_half_up((gct_qtr * {{ dart }} / ndip),
                                      digits = 2),

    # Ending AR Decrease Needed
    earb_dc = janitor::round_half_up({{ earb }} - earb_trg, digits = 2),

    # Ending AR Percentage Decrease Needed
    earb_pct = janitor::round_half_up(((earb_dc / {{ earb }}) * 100), digits = 2),

    # Boolean indicating whether DAR was under/over DARt
    pass = dplyr::case_when(dar < {{ dart }} ~ TRUE, TRUE ~ FALSE)) |>

    # Reorder columns
    dplyr::select(date,
                  nqtr,
                  ndip,
                  gct_qtr,
                  earb,
                  earb_trg,
                  earb_dc,
                  earb_pct,
                  adc,
                  dar,
                  pass,
                  actual,
                  ideal,
                  radiff)
}

#' Days in AR Example Data
#'
#' @keywords internal
#'
#' @autoglobal
#'
#' @export
dar_example <- function() {

  dplyr::tibble(
    date = as.Date(c(
      "2024-01-01",
      "2024-02-01",
      "2024-03-01",
      "2024-04-01",
      "2024-05-01",
      "2024-06-01",
      "2024-07-01",
      "2024-08-01",
      "2024-09-01",
      "2024-10-01",
      "2024-11-01",
      "2024-12-01"
      )
    ),

    gct = c(
      325982.23,
      297731.74,
      198655.14,
      186047.56,
      123654.34,
      131440.28,
      153991.95,
      156975.52,
      146878.12,
      163799.44,
      151410.74,
      169094.46
      ),

    earb = c(
      288432.52,
      307871.08,
      253976.56,
      183684.92,
      204227.59,
      203460.47,
      182771.32,
      169633.64,
      179347.72,
      178051.11,
      162757.49,
      199849.32
      )
    )
}
