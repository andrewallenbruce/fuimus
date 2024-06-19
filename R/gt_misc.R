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
