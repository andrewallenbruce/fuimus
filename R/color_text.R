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
