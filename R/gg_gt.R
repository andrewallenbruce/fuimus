#' gt Marks
#'
#' @param gt_tbl A `<gt_tbl>` object
#'
#' @param cols columns to target
#'
#' @returns A `<gt_tbl>` object with marks
#'
#' @examples
#' dplyr::tibble(pass = c(TRUE, FALSE, TRUE, FALSE)) |>
#'   gt::gt() |>
#'   gt_marks(cols = pass)
#'
#' @export
#'
#' @autoglobal
gt_marks <- function(gt_tbl, cols) {

  gt_tbl |>
    gt::text_case_when(
      x == TRUE ~ gt::html(
        fontawesome::fa(
          "check",
          prefer_type = "solid",
          fill = "red"
        )
      ),
      x == FALSE ~ gt::html(
        fontawesome::fa(
          "xmark",
          prefer_type = "solid",
          fill = "grey40"
        )
      ),
      .default = NA,
      .locations = gt::cells_body(
        columns = {{ cols }}))
}

#' Color text using HTML color styling
#'
#' Useful in conjuction with ggtext and glue
#'
#' @param text `<character>` string of text to
#'
#' @param hex_code `<character>` string, color hex code
#'
#' @returns `<character>` string
#'
#' @examples
#' color_text("Hello there!", "#BD43BF")
#'
#' @autoglobal
#'
#' @export
color_text <- function(text, hex_code) {

  paste0(
    "<span style='color:",
    hex_code,
    "'>",
    text,
    "</span>"
  )

}

#' Sorted Bar Chart
#'
#' @param df data frame
#'
#' @param var column to plot
#'
#' @returns A [ggplot2][ggplot2::ggplot2-package] object
#'
#' @examples
#' ggplot2::diamonds |>
#'   sorted_bars(cut) +
#'   gg_theme()
#'
#' @autoglobal
#'
#' @export
sorted_bars <- function(df, var) {

  df |>
    dplyr::mutate(
      {{ var }} := forcats::fct_rev(
        forcats::fct_infreq(
          {{ var }})
      )
    )  |>
    ggplot2::ggplot(
      ggplot2::aes(
        y = {{ var }}
      )
    ) +
    ggplot2::geom_bar()
}

#' Histogram
#'
#' @param df data frame
#'
#' @param var var
#'
#' @param binwidth `<numeric>` binwidth
#'
#' @returns ggplot2 histogram
#'
#' @examples
#' ggplot2::diamonds |>
#'   histogram(carat, 0.1)
#'
#' @autoglobal
#'
#' @export
histogram <- function(df,
                      var,
                      binwidth) {
  label <- rlang::englue("A histogram of {{var}} with binwidth {binwidth}")

  df |>
    ggplot2::ggplot(
      ggplot2::aes(
        {{ var }}
      )
    ) +
    ggplot2::geom_histogram(binwidth = binwidth) +
    ggplot2::labs(title = label)
}

#' Density plot
#'
#' @param df df
#'
#' @param var var
#'
#' @param color color var
#'
#' @param facets facets var
#'
#' @param binwidth binwidth
#'
#' @returns ggplot2 density plot
#'
#' @examples
#' ggplot2::diamonds |> density(carat)
#'
#' ggplot2::diamonds |> density(carat, cut)
#'
#' ggplot2::diamonds |> density(carat, cut, clarity)
#'
#' @autoglobal
#'
#' @export
density <- function(df,
                    var,
                    color,
                    facets,
                    binwidth = 0.1) {
  df |>
    ggplot2::ggplot(
      ggplot2::aes(
        {{ var }},
        ggplot2::after_stat(density),
        color = {{ color }}
      )
    ) +
    ggplot2::geom_freqpoly(
      binwidth = binwidth
    ) +
    ggplot2::facet_wrap(
      ggplot2::vars(
        {{ facets }}
      )
    )
}

#' Plots a fancy time series
#'
#' @param df data.frame
#'
#' @param val value var
#'
#' @param group group var
#'
#' @returns A [tibble][tibble::tibble-package] with the summarized data
#'
#' @examples
#' df <- dplyr::tibble(
#'    dist1 = sort(rnorm(50, 5, 2)),
#'    dist2 = sort(rnorm(50, 8, 3)),
#'    dist4 = sort(rnorm(50, 15, 1)),
#'    date = seq.Date(
#'       as.Date("2022-01-01"),
#'       as.Date("2022-04-10"),
#'       by = "2 days")) |>
#' tidyr::pivot_longer(
#'    cols = -date,
#'    names_to = "dist_name",
#'    values_to = "value")
#'
#' df
#'
#' fancy_ts(df, value, dist_name) +
#' gg_theme()
#'
#' @export
#'
#' @autoglobal
fancy_ts <- function(df, val, group) {

  labs <- df |>
    dplyr::group_by({{ group }}) |>
    dplyr::summarize(breaks = max({{ val }}))

  df |>
    ggplot2::ggplot(
      ggplot2::aes(date, {{ val }},
                   group = {{ group }},
                   color = {{ group }})) +
    ggplot2::geom_path() +
    ggplot2::scale_y_continuous(
      breaks = labs$breaks,
      labels = scales::label_comma(),
      minor_breaks = NULL,
      guide = ggplot2::guide_axis(position = "right"))
}

#' ggplot2 theme
#'
#' @returns ggplot theme
#'
#' @examples
#' ggplot2::diamonds |>
#' histogram(carat, 0.1) +
#' gg_theme()
#'
#' @autoglobal
#'
#' @export
gg_theme <- function() {

  ggplot2::theme_minimal(
    # base_family = "Roboto Condensed"
  ) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        size = ggplot2::rel(1.5),
        face = "bold"
      ),
      plot.subtitle = ggplot2::element_text(
        size = ggplot2::rel(1.1)
      ),
      plot.caption = ggplot2::element_text(
        color = "#777777",
        vjust = 0
      ),
      axis.title = ggplot2::element_text(
        size = ggplot2::rel(.9),
        hjust = 0.95,
        face = "italic"
      ),
      panel.grid.major = ggplot2::element_line(
        size = ggplot2::rel(.1),
        color = "#000000"
      ),
      panel.grid.minor = ggplot2::element_line(
        size = ggplot2::rel(.05),
        color = "#000000"
      ),
      legend.position = "none"
    )
}

# gg_labs <- function() {
#   labs(title = "Average miles per gallon by number of cylinders",
#        subtitle = "Scatter plot depicting average miles per gallon aggregated by number of cylinders",
#        x = "Number of cylinders",
#        y = "Miles per gallon",
#        caption = "Source: Estimates calculated from the 'mtcars' data set")
# }
# gg_save <- function(file) {
#   ggsave(file, width = 7, height = 4.5, units = "in")
# }
