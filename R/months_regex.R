#' Regular expression for matching month names
#'
#' @returns `<chr>` string  of a regex for matching month names
#'
#' @examples
#' months_regex()
#'
#' @autoglobal
#'
#' @export
months_regex <- function() {
  single_line_string(
    "(Jan(?:uary)?|Feb(?:ruary)?|Mar(?:ch)?|Apr(?:il)?|May|Jun(?:e)?|
     Jul(?:y)?|Aug(?:ust)?|Sep(?:tember)?|Oct(?:ober)?|Nov(?:ember)?|
     Dec(?:ember)?)\\s+(\\d{1,2})\\,\\s+(\\d{4})")
}
