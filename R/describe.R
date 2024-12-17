#' Describe 2
#'
#' @param df `<data.frame>` desc
#'
#' @param ... `<dots>` tidyselect columns
#'
#' @returns `<tibble>` of summary statistics
#'
#' @examples
#' describe2(mock_provider(2000:2020))
#'
#' describe2(mock_forager(200))
#'
#' @autoglobal
#'
#' @export
describe2 <- function(df, ...) {

  get_type <- \(x) {
    cheapr::enframe_(
      purrr::map_vec(x, function(x)
          glue_chr("<{pillar::type_sum(x)}>")),
      name = "column",
      value = "type")
  }

  fiqr <- \(x) diff(collapse::.quantile(as.numeric(x), c(0.25, 0.75)))

  if (nargs() > 1) df <- dplyr::select(df, ...)

  dates <- dplyr::select(df, dplyr::where(\(x) inherits(x, "Date")))
  df    <- dplyr::select(df, dplyr::where(\(x) !inherits(x, "Date")))

  sums <- df |>
    dplyr::mutate(
      dplyr::across(dplyr::where(is.character), stringr::str_length),
      dplyr::across(dplyr::where(\(x) is.factor(x) | is.logical(x)), as.numeric)) |>
    tidyr::pivot_longer(dplyr::everything(), names_to = "column") |>
    dplyr::mutate(n = 1 - cheapr::is_na(value)) |>
    dplyr::reframe(
      n = collapse::fsum(value, nthreads = 4L),
      min = collapse::fmin(value),
      mean = collapse::fmean(value, nthreads = 4L),
      iqr = fiqr(value),
      max = collapse::fmax(value),
      med = collapse::fmedian(value),
      sd = collapse::fsd(value),
      mad = mad(value, na.rm = TRUE),
      distribution = histo(value),
      .by = column) |>
    dplyr::left_join(get_type(df), by = dplyr::join_by(column))

  topn <- \(x, limit = 10) {
    dplyr::tibble(
      column = names(x),
      uniq = collapse::fnunique(collapse::na_rm(x)),
      top = collapse::fcount(collapse::na_rm(x), name = "n") |>
        dplyr::arrange(dplyr::desc(n)) |>
        dplyr::slice(seq(1, limit)) |>
        dplyr::pull(x) |>
        stringr::str_flatten_comma())
  }

  tops <- purrr::map(df, topn) |>
    purrr::list_rbind(names_to = "column") |>
    dplyr::filter(uniq != nrow(df))

  dplyr::left_join(sums, tops, by = dplyr::join_by(column)) |>
    dplyr::arrange(dplyr::desc(type)) |>
    dplyr::select(column, type, n, min, mean, med, max, iqr, sd, mad, distribution, uniq, top)
}

#' Describe a dataset
#'
#' @param df `<data.frame>` desc
#'
#' @param ... `<dots>` tidyselect columns
#'
#' @returns `<tibble>` of summary statistics
#'
#' @examples
#' describe(mock_provider(2000:2020))
#'
#' describe(mock_forager(200), !dplyr::starts_with("date"))
#'
#' @autoglobal
#'
#' @importFrom stats median IQR
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
      n = as.integer(sum(n, na.rm = TRUE)),
      mean = mean(value, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE),
      iqr = IQR(value, na.rm = TRUE),
      med = median(value, na.rm = TRUE),
      mad = mad(value, na.rm = TRUE),
      range = glue_chr(
        "[{roundup(min(value, na.rm = TRUE))} - {roundup(max(value, na.rm = TRUE))}]"
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
      nuniq = collapse::fnunique(collapse::na_rm(x)),
      top_n = collapse::fcount(collapse::na_rm(x), name = "n") |>
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
                  nuniq,
                  top_n)
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
    bins$counts / max(bins$counts, na.rm = TRUE),
    breaks = seq(0, 1, length = length(sparks) + 1),
    labels = sparks,
    include.lowest = TRUE
  )
  paste0(factor, collapse = "")
}


#' Describe unique values in a data frame
#'
#' @param df data frame
#'
#' @param ... unquoted data frame columns
#'
#' @param .rename_first default is `"value"`
#'
#' @param .set_names vector of names
#'
#' @param .names_to default is `"variable"`
#'
#' @returns a data frame with unique values
#'
#' @examples
#'
#' describe_unique(mock_forager(), class, payer)
#'
#' describe_unique(
#'   mock_forager(50),
#'   names(mock_forager())[c(2:3, 6:9)]) |>
#'   dplyr::filter(n > 2) |>
#'   print(n = 30)
#'
#' @autoglobal
#'
#' @export
describe_unique <- function(df,
                            ...,
                            .rename_first = "value",
                            .set_names = NULL,
                            .names_to = "variable") {

  rlang::check_dots_unnamed()

  df <- dplyr::select(df, ...)

  .set_names <- if (null(.set_names)) names(df) else .set_names

  df <- dplyr::mutate(df, dplyr::across(!dplyr::where(is.character), as.character))

  df <- columns_to_character(df) |>
    names() |>
    purrr::map(~ dplyr::count(df, .data[[.x]], sort = TRUE)) |>
    purrr::set_names(nm = .set_names)

  first_column_rename <- \(x, first = .rename_first) {

    for (i in 1:length(x)) {

      colnames(x[[i]])[1] <- first

    }
    x
  }

  first_column_rename(df) |>
    purrr::list_rbind(names_to = .names_to)
}
