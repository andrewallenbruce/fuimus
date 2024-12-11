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
#' @param package `<chr>` package name
#'
#' @returns `<pins_board_folder>` or `<pins_board_url>`
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
mount_board <- function(source = c("local", "remote"), package = "fuimus") {

  source <- match.arg(source)

  switch(
    source,
    local = pins::board_folder(
    # system.file("data", "zcta_crosswalk.rda", package = package)
    fs::path_package("extdata/pins", package = package)),
    remote = pins::board_url(
      gh_raw(
        glue::glue(
      "andrewallenbruce/{package}/master/inst/extdata/pins/")
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

#' Install/update personal packages
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
update_personal_packages <- function() {

  pkgs <- c(
    "careroll",
    "codexchain",
    "costoffice",
    "defogger",
    "forager",
    "fuimus",
    "northstar",
    "pathologie",
    "procedural",
    "provider",
    "nppez",
    "arkrvu",
    "arktax",
    "arknpi",
    "codex"
  )

  pak::pkg_install(
    glue::glue(
      "andrewallenbruce/{pkgs}"
    ),
    upgrade = TRUE,
    ask = FALSE)
}

#' Initialize a new R package
#'
#' @param pkgname `<chr>` name of package
#'
#' @returns nothing
#'
#' @examplesIf FALSE
#' initialize_package("package")
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
initialize_package <- \(pkgname) {

  local  <- "C:/Users/Andrew/Desktop/Repositories/"
  github <- "https://github.com/andrewallenbruce/"

  usethis::create_package(
    path = glue::glue("{local}{pkgname}"),
    fields = list(
      Package = glue::glue("{pkgname}"),
      "Authors@R" = utils::person(
        given = c("Andrew", "Allen"),
        family = "Bruce",
        email = "andrewallenbruce@gmail.com",
        role = c("aut", "cre", "cph")),
      Maintainer = "Andrew Allen Bruce <andrewallenbruce@gmail.com>",
      URL = glue::glue("{github}{pkgname}"),
      BugReports = glue::glue("{github}{pkgname}/issues"))
  )
}
