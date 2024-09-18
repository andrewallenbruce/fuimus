#' Construct regex patterns
#'
#' @param x `<chr>` vector
#'
#' @examples
#' # construct_regex(search_descriptions()$hcpcs_code)
#'
#' # construct_regex(search_adjustments()$adj_code)
#'
#' # Incorrect output:
#' "^[A-DM-PWY1-9][AIOR0-9]?[0-9]?{3}$"
#'
#' # Digits at the end should be:
#' "^[A-DM-PWY1-9][AIOR0-9]?[0-9]{0,3}?$"
#'
#' # Test adj codes
#' adj <- c("4", "CO", "P6", "100",
#'          "B19", "MA22", "MA124", "N766")
#'
#' construct_regex(adj)
#'
#' @returns `<chr>` regex vector
#'
#' @autoglobal
#'
#' @export
construct_regex <- function(x) {

  uniq_nona <- \(x) collapse::funique(collapse::na_rm(x))

  x <- gsub(" ", "", uniq_nona(x))

  vecs <- stringr::str_split_fixed(
    x, "",
    n = max(
      collapse::vlengths(x)
    )
  ) |>
    as.data.frame() |>
    purrr::map(na_if_common)

  to_brackets <- vecs |>
    purrr::map(uniq_nona) |>
    purrr::map(pos_re)

  qmark <- names(which(purrr::map_lgl(vecs, anyNA)))

  to_vec <- to_brackets |>
    purrr::map(id_runs)

  nobrack <- which(stringr::str_detect(to_vec, "\\[|\\]", negate = TRUE))

  to_vec[nobrack] <- purrr::map(to_vec[nobrack], \(x) paste0("[", x, "]"))

  if (!vctrs::vec_is_empty(qmark)) {
    to_vec[qmark] <- purrr::map(
      to_vec[qmark], \(x) paste0(x, "?"))
  }

  to_vec <- purrr::list_c(to_vec)

  if (collapse::any_duplicated(to_vec)) {

    # TODO THIS MUST COME BEFORE APPLYING QUESTION MARKS
    # probably need to vectorize this, will surely
    # have more than one unique duplicate out of order

    dupe_idx <- which(collapse::fduplicated(to_vec, all = TRUE))

    rp <- paste0(to_vec[dupe_idx][1], "{", length(dupe_idx), "}")

    to_vec[dupe_idx] <- rp

    to_vec <- collapse::funique(to_vec)

  }

  x <- paste0("^", fuimus::collapser(to_vec), "$")

  return(x)
}

#' Internal function for `construct_regex()`
#'
#' @param x `<chr>` vector
#'
#' @returns `<chr>` vector
#'
#' @autoglobal
#'
#' @noRd
pos_re <- function(x) {

  sorted   <- stringr::str_sort(x, numeric = TRUE)
  alphabet <- purrr::list_c(strex::str_extract_non_numerics(sorted))
  numbers  <- purrr::list_c(strex::str_extract_numbers(sorted))

  paste0(
    fuimus::collapser(alphabet),
    fuimus::collapser(numbers)
  )
}

#' Internal function for `construct_regex()`
#'
#' @param x `<chr>` vector
#'
#' @returns `<chr>` vector
#'
#' @autoglobal
#'
#' @noRd
id_runs <- function(x) {

  vec <- c(LETTERS, 0:9)

  vec <- rlang::set_names(rep(0, length(vec)), vec)

  test <- fuimus::splitter(x)

  vecna <- vec[test]

  vecna <- vecna[!is.na(vecna)]

  vec[names(vecna)] <- 1

  vec_group <- dplyr::tibble(
    value = names(vec),
    key = vec,
    idx = 1:length(vec),
    group = dplyr::consecutive_id(key)
  ) |>
    dplyr::mutate(
      group_size = dplyr::n(),
      .by = group
    ) |>
    dplyr::filter(
      key == 1,
      group_size >= 3
    ) |>
    dplyr::select(
      value,
      group
    )

  if (vctrs::vec_is_empty(vec_group)) return(x)

  xgroups <- unname(
    split(
      vec_group,
      vec_group$group
    )
  ) |>
    purrr::map(
      purrr::pluck("value")
    ) |>
    purrr::map(
      paste0,
      collapse = ""
    ) |>
    purrr::list_c()

  replacements <- dplyr::left_join(
    dplyr::slice_min(
      vec_group,
      by = group,
      order_by = value
    ) |>
      dplyr::rename(start = value),
    dplyr::slice_max(
      vec_group,
      by = group,
      order_by = value
    ) |>
      dplyr::rename(end = value),
    by = dplyr::join_by(group)
  ) |>
    glue::glue_data(
      "{start}-{end}"
    ) |>
    as.vector()

  res <- stringi::stri_replace_all_regex(
    x,
    xgroups,
    replacements,
    vectorize_all = FALSE)

  paste0("[", res, "]")
}

#' Internal function for `construct_regex2()`
#'
#' @param x `<chr>` vector
#'
#' @returns `<chr>` vector
#'
#' @autoglobal
#'
#' @noRd
pos_nchar <- function(x) {

  ch <- range(collapse::vlengths(x))

  ifelse(
    ch[1] == ch[2],
    paste0("{", ch[1], "}"),
    paste0("{", ch[1], ",", ch[2], "}")
  )

}

#' Common Regular expressions
#'
#' @param x `<chr>` regex name
#'
#' @returns `<chr>` string of a regex
#'
#' @examples
#' common_regex("url")
#'
#' common_regex("month")
#'
#' common_regex("month_date")
#'
#' @autoglobal
#'
#' @export
common_regex <- function(x = c("month_date", "month", "url")) {

  x <- match.arg(x)

  reg <- list(
    month_date = single_line_string(
      "(Jan(?:uary)?|Feb(?:ruary)?|Mar(?:ch)?|Apr(?:il)?|May|Jun(?:e)?|
      Jul(?:y)?|Aug(?:ust)?|Sep(?:tember)?|Oct(?:ober)?|Nov(?:ember)?|
      Dec(?:ember)?)\\s+(\\d{1,2})\\,\\s+(\\d{4})"
    ),
    month = single_line_string(
      "(Jan(?:uary)?|Feb(?:ruary)?|Mar(?:ch)?|Apr(?:il)?|May|Jun(?:e)?|
      Jul(?:y)?|Aug(?:ust)?|Sep(?:tember)?|Oct(?:ober)?|Nov(?:ember)?|
      Dec(?:ember)?)"
    ),
    url = single_line_string(
      "^(?:(?:http(?:s)?|ftp)://)(?:\\S+(?::(?:\\S)*)?@)?(?:(?:[a-z0-9
      \u00a1-\uffff](?:-)*)*(?:[a-z0-9\u00a1-\uffff])+)(?:\\.(?:[a-z0-9
      \u00a1-\uffff](?:-)*)*(?:[a-z0-9\u00a1-\uffff])+)*(?:\\.(?:[a-z0-9
      \u00a1-\uffff]){2,})(?::(?:\\d){2,5})?(?:/(?:\\S)*)?$"
    )
  )

  reg[[x]]
}
