#' Calculate lagged values by column
#'
#' @param df data frame
#'
#' @param col column of numeric values to calculate lag
#'
#' @param by column to calculate lag by
#'
#' @examples
#' fuimus:::provider_data(2020:2025) |>
#'   dplyr::group_by(group) |>
#'   change_lagged(net_payment, year)
#'
#' @autoglobal
#'
#' @export
change_lagged <- function(df, col, by = NULL) {

  newcol <- rlang::englue("{{col}}_chg")
  newcol <- rlang::sym(newcol)

  df |>
    dplyr::mutate(
      "{{ col }}_chg" := {{ col }} - dplyr::lag({{ col }},
                                                order_by = {{ by }}),
      "{{ col }}_pct" := !!newcol / dplyr::lag({{ col }},
                                               order_by = {{ by }}),
      .after = {{ col }})
}

#' Calculate Geometric Mean
#'
#' Average [rate_of_return()]
#'
#' @param x numeric vector
#'
#' @examples
#' x <- fuimus:::provider_data(2020:2025)
#'
#' x |>
#' dplyr::group_by(group) |>
#' rate_of_return(net_payment)
#'
#' x |>
#' dplyr::group_by(group) |>
#' rate_of_return(net_payment) |>
#' dplyr::summarise(gmean = geomean(net_payment_ror),
#' .by = group)
#'
#' @autoglobal
#'
#' @export
geomean <- function(x) {
  exp(
    mean(
      log(x),
      na.rm = TRUE
    )
  )
}

#' Lagged rate of return
#'
#' @param df data frame
#'
#' @param col numeric column
#'
#' @param n `<int>` values to offset
#'
#' @param fill_na `<int>` fill value for any NAs; default is 1
#'
#' @examples
#' fuimus:::provider_data(2020:2025) |>
#'   dplyr::group_by(group) |>
#'   rate_of_return(net_payment)
#'
#' @autoglobal
#'
#' @export
rate_of_return <- function(df, col, n = 1, fill_na = 1L) {

  res <- df |>
    dplyr::mutate(
      copy = dplyr::if_else(
        {{ col }} == 0, 1, {{ col }}
        ),
      lg = dplyr::lag(copy, n = n),
      "{{ col }}_ror" := copy / lg,
      copy = NULL,
      lg = NULL,
      .after = {{ col }}
      )

  if (!is.na(fill_na)) {
    res[is.na(res)] <- fill_na
  }

  if (dplyr::is_grouped_df(df)) {
    res <- dplyr::ungroup(res)
  }

  return(res)
}

#' Lagged absolute change
#'
#' @param x numeric vector
#'
#' @param n values to offset
#'
#' @param fill_na fill value for any NAs; default is 0
#'
#' @examples
#' fuimus:::provider_data(2020:2025) |>
#'   dplyr::mutate(change = chg_abs(net_payment),
#'   .by = group)
#'
#' @autoglobal
#'
#' @export
chg_abs <- function(x, n = 1L, fill_na = 0L) {

  lg  <- dplyr::lag(x, n = n)

  res <- (x - lg)

  if (!is.na(fill_na)) {

    res[is.na(res)] <- fill_na

    }

  return(res)
}

#' Lagged percentage change
#'
#' @param x numeric vector
#'
#' @param n values to offset
#'
#' @param fill_na fill value for any NAs; default is 0
#'
#' @examples
#' fuimus:::provider_data(2020:2025) |>
#'   dplyr::mutate(change = chg_pct(net_payment),
#'   .by = group)
#'
#' @autoglobal
#'
#' @export
chg_pct <- function(x, n = 1L, fill_na = 0L) {

  lg <- dplyr::lag(x, n = n)

  res <- (x - lg) / lg

  if (!is.na(fill_na)) {
    res[is.na(res)] <- fill_na
    }

  return(res)
}

#' Calculate Lagged Metrics
#'
#' @param df data frame
#'
#' @param cols numeric columns to calculate absolute/relative change & rate of return
#'
#' @param csm numeric cols to calculate cumulative sum for
#'
#' @examples
#' fuimus:::provider_data(2020:2025) |>
#'   dplyr::group_by(group) |>
#'   change(net_payment, csm = c("payment", "_chg"))
#'
#' @autoglobal
#'
#' @export
change <- function(df, cols, csm = NULL) {

  results <- df |>
    dplyr::mutate(
      dplyr::across(
        {{ cols }},
        list(
          chg = \(x) chg_abs(x),
          pct = \(x) chg_pct(x)
          ),
        .names = "{.col}_{.fn}")
      ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::ends_with("_pct"), ~ .x + 1,
        .names = "{.col}_ror")
      ) |>
    dplyr::relocate(
      dplyr::ends_with("_chg"),
      dplyr::ends_with("_pct"),
      dplyr::ends_with("_pct_ror"),
      .after = {{ cols }}
      )

  names(results) <- gsub(
    "_pct_ror",
    "_ror",
    names(results)
    )

  if (!is.null(csm)) {

    results <- results |>
      dplyr::mutate(
        dplyr::across(
          dplyr::ends_with({{ csm }}),
          list(csm = \(x) cumsum(x)
               ),
          .names = "{.col}_{.fn}")
        )
  }

  if (dplyr::is_grouped_df(results)) {
    results <- dplyr::ungroup(results)
  }

  return(results)
}
