#' gt Marks
#'
#' @param gt_tbl description
#'
#' @param cols description
#'
#' @returns description
#'
#' @export
#'
#' @keywords internal
#'
#' @autoglobal
gt_marks <- function(gt_tbl, cols) {

  gt_tbl |>
    gt::text_case_when(
      x == TRUE ~ gt::html(
        fontawesome::fa("check",
                        prefer_type = "solid",
                        fill = "red")),
      x == FALSE ~ gt::html(
        fontawesome::fa("xmark",
                        prefer_type = "solid",
                        fill = "white")),
      .default = NA,
      .locations = gt::cells_body(
        columns = {{ cols }}))
}
