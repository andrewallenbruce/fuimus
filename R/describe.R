#' Describe a dataset
#'
#' @param df `<data.frame>` desc
#'
#' @param ... `<dots>` tidyselect columns
#'
#' @returns `<tibble>` of summary statistics
#'
#' @examples
#' describe(fuimus:::provider_data(2000:2020))
#'
#' describe(fuimus:::forager_data(200), !dplyr::starts_with("date"))
#'
#' @autoglobal
#'
#' @export
describe <- function(df, ...) {

  if (nargs() > 1) df <- dplyr::select(df, ...)

  df_sums <- df |>
    dplyr::mutate_if(is.character, stringr::str_length) |>
    dplyr::mutate_if(is.factor, as.numeric) |>
    dplyr::mutate_if(is.logical, as.numeric) |>
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "variable"
      ) |>
    dplyr::mutate(n = 1 - is.na(value)) |>
    dplyr::reframe(
      n = as.integer(sum_na(n)),
      mean = mean_na(value),
      sd = sd_na(value),
      iqr = iqr_na(value),
      med = median_na(value),
      mad = mad_na(value),
      range = glue_chr(
        "[{roundup(min_na(value))} - {roundup(max_na(value))}]"
        ),
      histogram = histo(value),
      .by = variable
    )

  get_type <- \(x) {
    dplyr::tibble(
      variable = names(x),
      type = stringr::str_c("<", pillar::type_sum(x), ">") |>
        forcats::as_factor())
  }

  df_types <- purrr::map(df, get_type) |>
    purrr::list_rbind(names_to = "variable")

  get_unique <- \(x, limit = 5) {
    dplyr::tibble(
      variable = names(x),
      n_uniq = collapse::fnunique(collapse::na_rm(x)),
      top_5 = collapse::fcount(collapse::na_rm(x), name = "n") |>
        dplyr::arrange(dplyr::desc(n)) |>
        dplyr::slice(1:limit) |>
        dplyr::pull(x) |>
        stringr::str_flatten_comma())
  }

  df_unique <- purrr::map(df, get_unique) |>
    purrr::list_rbind(names_to = "variable")

  joinby <- dplyr::join_by(variable)

  dplyr::left_join(df_types, df_sums, by = joinby) |>
    dplyr::left_join(df_unique, by = joinby) |>
    dplyr::arrange(dplyr::desc(type)) |>
    dplyr::select(variable,
                  type,
                  range,
                  histogram,
                  mean,
                  sd,
                  iqr,
                  med,
                  mad,
                  n,
                  n_uniq,
                  top_5)
}

#' Inline histogram
#'
#' @param x description
#'
#' @param width description
#'
#' @returns description
#'
#' @examples
#' histo(x = rnorm(100))
#'
#' histo(x = runif(100))
#'
#' histo(x = rnorm(100), width = 5)
#'
#' histo(x = runif(100), width = 5)
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
histo <- function(x, width = 10) {

  sparks <- c("\u2581", "\u2582", "\u2583", "\u2585", "\u2587")

  bins <- graphics::hist(x, breaks = width, plot = FALSE)

  factor <- cut(
    bins$counts / max(bins$counts),
    breaks = seq(0, 1, length = length(sparks) + 1),
    labels = sparks,
    include.lowest = TRUE
  )

  paste0(factor, collapse = "")
}
