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
#' describe(
#'    fuimus:::forager_data(200),
#'    !dplyr::starts_with("date")
#' )
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
      n      = as.integer(base::sum(n)),
      amean  = base::mean(value, na.rm = TRUE),
      gmean  = fuimus::geomean(value),
      sd     = stats::sd(value, na.rm = TRUE),
      iqr    = stats::IQR(value, na.rm = TRUE),
      median = stats::median(value, na.rm = TRUE),
      mad    = stats::mad(value, na.rm = TRUE),
      range  = as.character(stringr::str_glue(
          "[",
          "{base::min(value, na.rm = TRUE)}",
          " - ",
          "{as.integer(base::max(value, na.rm = TRUE))}",
          "]"
        )
      ),
      hist = cheapr:::inline_hist(value),
      .by = variable
    )

  get_type <- \(x) dplyr::tibble(
    variable = names(x),
    type = stringr::str_c("<", pillar::type_sum(x), ">") |>
      forcats::as_factor()
  )

  df_types <- purrr::map(df, get_type) |>
    purrr::list_rbind(names_to = "variable")

  get_unique <- \(x, limit = 5) dplyr::tibble(
    variable = names(x),
    n_uniq = collapse::fnunique(collapse::na_rm(x)),
    top_5 = collapse::fcount(collapse::na_rm(x), name = "n") |>
      dplyr::arrange(dplyr::desc(n)) |>
      dplyr::slice(1:limit) |>
      dplyr::pull(x) |>
      stringr::str_flatten_comma()
  )

  df_unique <- purrr::map(df, get_unique) |>
    purrr::list_rbind(names_to = "variable")

  joinby <- dplyr::join_by(variable)

  dplyr::left_join(df_types, df_sums, by = joinby) |>
    dplyr::left_join(df_unique, by = joinby) |>
    dplyr::arrange(dplyr::desc(type))
}
