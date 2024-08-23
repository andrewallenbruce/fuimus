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
#' x <- fuimus:::forager_data(200)
#'
#' x
#'
#' describe_unique(x, claim_id, payer)
#'
#' @autoglobal
#'
#' @export
describe_unique <- function(df,
                            ...,
                            .rename_first = "value",
                            .set_names = NULL,
                            .names_to = "variable") {

  df <- fuimus::df_2_chr(df)
  df <- dplyr::select(df, ...) |>
    names() |>
    purrr::map(~ dplyr::count(
      df,
      .data[[.x]],
      sort = TRUE)
      ) |>
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
