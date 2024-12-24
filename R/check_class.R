#' Check Class of Argument Inputs
#'
#' @param ... Objects to be checked for class.
#'
#' @param .expected_class Character. The name of the class against which objects
#'     should be checked.
#'
#' @param .call The environment in which this function is to be called.
#'
#' @keywords internal
#'
#' @autoglobal
#'
#' @export
check_class <- function(...,
                        .expected_class = c("numeric", "character"),
                        .call = rlang::caller_env()) {

  .expected_class <- match.arg(.expected_class)

  args <- rlang::dots_list(..., .named = TRUE)

  # Check each value against expected class
  args_are_class <- lapply(args, \(arg) {
      switch(.expected_class,
             numeric   = is.numeric(arg),
             character = is.character(arg)
      )}
    )

  # Isolate values with wrong class
  fails_names <- names(
    Filter(
      isFALSE,
      args_are_class
      )
    )

  if (length(fails_names) > 0) {

    # Prepare variables with failure information
    fails         <- args[names(args) %in% fails_names]
    fails_classes <- sapply(fails, class)

    # Build a bulleted vector of failures

    fails_bullets <- rlang::set_names(
      paste0(
        "{.var ",
        names(fails_classes),
        "} with class {.cls ",
        fails_classes,
        "}"
      ),
      rep("*", length(fails_classes)))

    cli::cli_abort(
      message = c(
        "{.var {fails_names}} must be of class {.cls {(.expected_class)}}",
        x = "You provided:", fails_bullets
      ),
      call = .call
    )
  }

}

#' @noRd
print_tree <- function(x) {

  nm <- rlang::englue("{{ x }}")

  writeLines(sprintf("█─ %s <%s>", nm, pillar::type_sum(x)))

  print_tree_impl(x, 0)
}

#' @noRd
print_tree_impl <- function(x, n_indent) {

  indent <- strrep(" ", n_indent)

  for (nm in names(x)) {

    if (is.list(x[[nm]])) { # || is.data.frame(x[[nm]])

      writeLines(sprintf("%s├─█─ %s <%s>", indent, nm, pillar::type_sum(x[[nm]])))

      print_tree_impl(x[[nm]], n_indent = n_indent + 2)

      next
    }
    writeLines(sprintf("%s├─── %s <%s>", indent, nm, pillar::type_sum(x[[nm]])))
  }
}
