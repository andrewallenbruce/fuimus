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
#' @keywords internal
#'
#' @export
gh_raw <- function(x) {
  paste0("https://raw.githubusercontent.com/", x)
}

#' Mount [pins][pins::pins-package] board
#'
#' @param source `<chr>` `"local"` or `"remote"`
#'
#' @returns `<pins_board_folder>` or `<pins_board_url>`
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
mount_board <- function(source = c("local", "remote")) {

  source <- match.arg(source)

  switch(
    source,
    local = pins::board_folder(
    # system.file("data", "zcta_crosswalk.rda", package = package)
    fs::path_package("extdata/pins", package = utils::packageName())),
    remote = pins::board_url(
      gh_raw(
        glue::glue(
      "andrewallenbruce/{utils::packageName()}/master/inst/extdata/pins/")
      )
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
#' @keywords internal
#'
#' @export
get_pin <- function(pin, ...) {

  board <- mount_board(...)

  pin <- rlang::arg_match0(pin, list_pins())

  pins::pin_read(board, pin)

}

#' List pins from a [pins][pins::pins-package] board
#'
#' @param ... arguments to pass to [mount_board()]
#'
#' @returns `<chr>` vector of [pins][pins::pins-package]
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
list_pins <- function(...) {

  board <- mount_board(...)

  pins::pin_list(board)

}
