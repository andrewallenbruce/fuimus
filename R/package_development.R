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

#' Commit and push changes with gert
#'
#' @param msg `<chr>` commit message
#'
#' @returns nothing
#'
#' @examplesIf FALSE
#' git_commit("* bump dev version")
#'
#' @autoglobal
#'
#' @keywords internal
#'
#' @export
git_commit <- \(msg) {

  files <- gert::git_status()

  files_to_stage <- files |>
    dplyr::filter(!staged) |>
    dplyr::pull(file)

  if (fs::file_exists(here::here(".git/index.lock"))) {
    fs::file_delete(here::here(".git/index.lock"))
  }

  gert::git_add(files_to_stage)
  gert::git_commit_all(message = msg)
  gert::git_push()

}
