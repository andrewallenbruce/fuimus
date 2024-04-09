#' Generate a sequence of numbers with a new prefix
#'
#' @param n `<int>` Numeric sequence to generate
#'
#' @param new `<chr>` New prefix
#'
#' @param between `<chr>` Separator between `new` and `old`, default `" = "`
#'
#' @param old `<chr>` Old prefix
#'
#' @param collapse `<chr>` Separator between sequences, default `", "`
#'
#' @param enclose `<chr>` *(optional)* Vector of `length(x) == 2` with which to enclose output
#'
#' @param style `<lgl>` Apply `styler::style_text()` to output, default `TRUE`
#'
#' @returns `<chr>` collapsed vector of `n` sequences
#'
#' @examples
#' rename_seq(
#' n        = 10,
#' new      = "id_issuer_",
#' between  = " = ",
#' old      = "Other.ID.Issuer.",
#' enclose  = c("x = c(", ")"),
#' collapse = ",\n ",
#' style    = TRUE)
#'
#' @autoglobal
#'
#' @export
rename_seq <- function(n,
                       new,
                       between = " = ",
                       old,
                       collapse = ", ",
                       enclose = NULL,
                       style = TRUE) {

  x <- stringr::str_c(
    new,
    seq(n),
    between,
    old,
    seq(n),
    collapse = collapse)

  if (!is.null(enclose)) {
    x <- stringr::str_c(
      enclose[1],
      x,
      enclose[2]
    )
  }

  if (style) {
    x <- styler::style_text(x)
  }
  return(x)
}
