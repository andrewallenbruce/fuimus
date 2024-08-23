#' Describe unique values in a data frame
#'
#' @param df data frame
#'
#' @param ... unquoted data frame columns
#'
#' @param .rename_first default is `"value"`
#'
#' @param .set_names vector of names
#'
#' @param .names_to default is `"variable"`
#'
#' @returns a data frame with unique values
#'
#' @examples
#'
#' describe_unique(fuimus:::forager_data(), claim_id, payer)
#'
#' # describe_unique(fuimus:::forager_data(200), names(df)[2:3])
#'
#' @autoglobal
#'
#' @export
describe_unique <- function(df,
                            ...,
                            .rename_first = "value",
                            .set_names = NULL,
                            .names_to = "variable") {

  rlang::check_dots_unnamed()

  df <- dplyr::select(df, ...)

  .set_names <- if (is.null(.set_names)) names(df) else .set_names

  df <- fuimus::df_2_chr(df) |>
    names() |>
    purrr::map(~ dplyr::count(df, .data[[.x]], sort = TRUE)) |>
    purrr::set_names(nm = .set_names)

  first_column_rename <- \(x, first = .rename_first) {

    for (i in 1:length(x)) {

      colnames(x[[i]])[1] <- first

    }
    x
  }

  first_column_rename(df) |>
    purrr::list_rbind(names_to = .names_to)
}
