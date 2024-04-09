#' @autoglobal
#'
#' @noRd
months_regex <- function() {
  single_line_string(
    "(Jan(?:uary)?|Feb(?:ruary)?|Mar(?:ch)?|Apr(?:il)?|May|Jun(?:e)?|
     Jul(?:y)?|Aug(?:ust)?|Sep(?:tember)?|Oct(?:ober)?|Nov(?:ember)?|
     Dec(?:ember)?)\\s+(\\d{1,2})\\,\\s+(\\d{4})")
}
