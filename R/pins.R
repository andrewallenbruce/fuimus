#' Return GitHub raw url
#'
#' @param x `<chr>` string
#'
#' @returns `<chr>` GitHub raw url
#'
#' @examples
#' gh_raw("andrewbruce/example/main/inst/pins/")
#'
#' @autoglobal
#'
#' @noRd
gh_raw <- function(x) {
  paste0("https://raw.githubusercontent.com/", x)
}

#' Mount [pins][pins::pins-package] board
#'
#' @param source `<chr>` `"local"` or `"remote"`
#'
#' @param package `<chr>` package name
#'
#' @returns `<pins_board_folder>` or `<pins_board_url>`
#'
#' @autoglobal
#'
#' @noRd
mount_board <- function(source = c("local", "remote"), package = "fuimus") {

  source <- match.arg(source)

  switch(
    source,
    local = pins::board_folder(
    fs::path_package("extdata/pins", package = package)),
    remote = pins::board_url(gh_raw(glue::glue(
      "andrewallenbruce/{package}/master/inst/extdata/pins/"))
    )
  )
}

#' Get a pinned dataset from a [pins][pins::pins-package] board
#'
#' @param pin `<chr>` string name of pinned dataset
#'
#' @param ... additional arguments passed to `mount_board()`
#'
#' @returns `<tibble>` or `<data.frame>`
#'
#' @autoglobal
#'
#' @noRd
get_pin <- function(pin, ...) {

  board <- mount_board(...)

  pin <- rlang::arg_match0(pin, list_pins())

  pins::pin_read(board, pin)

}

#' List pins from a [pins][pins::pins-package] board
#'
#' @param ... arguments to pass to [mount_board()]
#'
#' @returns `<list>` of [pins][pins::pins-package]
#'
#' @autoglobal
#'
#' @noRd
list_pins <- function(...) {

  board <- mount_board(...)

  pins::pin_list(board)

}
