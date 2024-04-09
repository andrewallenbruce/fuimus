#' Format multiple line character vector to single line
#'
#' @param x `<chr>` character vector with line breaks (`\n`)
#'
#' @returns `<chr>` single line character vector
#'
#' @examples
#' single_line_string(
#' "(Jan(?:uary)?|Feb(?:ruary)?|Mar(?:ch)?|
#' Apr(?:il)?|May|Jun(?:e)?|Jul(?:y)?|Aug(?:ust)?|
#' Sep(?:tember)?|Oct(?:ober)?|Nov(?:ember)?|
#' Dec(?:ember)?)\\s+(\\d{1,2})\\,\\s+(\\d{4})")
#'
#' @autoglobal
#'
#' @export
single_line_string <- function(x) {

  stringr::str_remove_all(
    x,
    r"(\n\s*)"
  )

}
