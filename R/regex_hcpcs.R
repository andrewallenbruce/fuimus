#' @noRd
pull_char <- function(x) {
  stringr::str_extract_all(
    x,
    stringr::regex("[A-Z]")
    )
}

#' @noRd
pull_numb <- function(x) {
  stringr::str_extract_all(
    x,
    stringr::regex("[0-9]")
    )
}

#' @noRd
sort_order <- function(x) {

  sorted   <- stringr::str_sort(x, numeric = TRUE)
  alphabet <- purrr::list_c(pull_char(sorted))
  numbers  <- purrr::list_c(pull_numb(sorted))

  paste0(
    fuimus::collapser(alphabet),
    fuimus::collapser(numbers)
    )
}

#' @noRd
reduce_runs <- function(x) {

  vec   <- rlang::set_names(rep(0, 36), c(LETTERS, 0:9))

  vec2 <- vec[fuimus::splitter(x)]

  vec2 <- vec2[!is.na(vec2)]

  vec[names(vec2)] <- 1

  vec_group <- dplyr::tibble(
    value = names(vec),
    key = vec,
    idx = seq_along(vec),
    group = dplyr::consecutive_id(key)) |>
    dplyr::mutate(
      group_size = dplyr::n(),
      .by = group) |>
    dplyr::filter(
      key == 1,
      group_size >= 3) |>
    dplyr::select(value, group)

  if (vctrs::vec_is_empty(vec_group)) return(x)

  xgroups <- unname(
    split(
      vec_group,
      vec_group$group
      )) |>
    purrr::map(purrr::pluck("value")) |>
    purrr::map(fuimus::collapser) |>
    purrr::list_c()

  replacements <- dplyr::left_join(
    dplyr::slice_min(vec_group, by = group, order_by = value) |> dplyr::rename(start = value),
    dplyr::slice_max(vec_group, by = group, order_by = value) |> dplyr::rename(end = value),
    by = dplyr::join_by(group)) |>
    glue::glue_data("{start}-{end}") |>
    as.vector()

  res <- stringi::stri_replace_all_regex(x, xgroups, replacements, vectorize_all = FALSE)

  paste0("[", res, "]")
}
#' @noRd
reduce_runs2 <- function(x) {

  test <- list(
    char = purrr::map(x, pull_char) |> purrr::list_c() |> purrr::compact() |> purrr::list_c(),
    numb = purrr::map(x, pull_numb) |> purrr::list_c() |> purrr::compact() |> purrr::list_c())

  vec <- list(
    char = rlang::set_names(rep(0, 26), LETTERS),
    numb = rlang::set_names(rep(0, 10), as.character(0:9)))

  vna <- list(
    char = vec$char[test$char],
    numb = vec$numb[test$numb])

  vna <- list(
    char = vna$char[!is.na(vna$char)],
    numb = vna$numb[!is.na(vna$numb)])

  vec$char[names(vna$char)] <- 1
  vec$numb[names(vna$numb)] <- 1

  group_char <- dplyr::tibble(
    value = names(vec$char),
    key = vec$char,
    idx = seq_along(vec$char),
    group = dplyr::consecutive_id(key)) |>
    dplyr::mutate(group_size = dplyr::n(), .by = group) |>
    dplyr::filter(key == 1, group_size >= 3) |>
    dplyr::select(value, group)

  group_numb <- dplyr::tibble(
    value = names(vec$numb),
    key = vec$numb,
    idx = seq_along(vec$numb),
    group = dplyr::consecutive_id(key)) |>
    dplyr::mutate(group_size = dplyr::n(), .by = group) |>
    dplyr::filter(key == 1, group_size >= 3) |>
    dplyr::select(value, group)

  xgroups_char <- unname(split(group_char, group_char$group)) |>
    purrr::map(purrr::pluck("value")) |>
    purrr::map(paste0, collapse = "") |>
    purrr::list_c()

  xgroups_numb <- unname(split(group_numb, group_numb$group)) |>
    purrr::map(purrr::pluck("value")) |>
    purrr::map(paste0, collapse = "") |>
    purrr::list_c()

  replace_char <- dplyr::left_join(
    dplyr::slice_min(group_char, by = group, order_by = value) |> dplyr::rename(start = value),
    dplyr::slice_max(group_char, by = group, order_by = value) |> dplyr::rename(end = value),
    by = dplyr::join_by(group)) |>
    glue::glue_data("{start}-{end}") |>
    as.vector()

  replace_numb <- dplyr::left_join(
    dplyr::slice_min(group_numb, by = group, order_by = value) |> dplyr::rename(start = value),
    dplyr::slice_max(group_numb, by = group, order_by = value) |> dplyr::rename(end = value),
    by = dplyr::join_by(group)) |>
    glue::glue_data("{start}-{end}") |>
    as.vector()

  orig <- list(
    char = fuimus::collapser(test$char),
    numb = fuimus::collapser(test$numb))

  res <- list(
    char = if(!vctrs::vec_is_empty(group_char)) {
      stringi::stri_replace_all_regex(
        orig$char,
        xgroups_char,
        replace_char,
        vectorize_all = FALSE)
    } else {
      orig$char
    },
    numb = if(!vctrs::vec_is_empty(group_numb)) {
      stringi::stri_replace_all_regex(
        orig$numb,
        xgroups_numb,
        replace_numb,
        vectorize_all = FALSE)
    } else {
      orig$numb
    }
  )
  paste0("[", res$char, res$numb, "]")
}

#' @noRd
group_split_length <- function(x) {

  x <- gsub("\\*", "", x)
  x <- gsub(" ", "", x)
  x <- fuimus::uniq_rmna(x)
  x <- collapse::rsplit(x, collapse::vlengths(x))
  names(x) <- paste0("x", names(x))
  x
}

#' @noRd
group_hcpcs_1 <- function(x) {

  if (!rlang::has_name(x, "x1")) return(NULL)

  fuimus::uniq_rmna(x[["x1"]])

}

#' @noRd
group_hcpcs_2 <- function(x) {

  if (!rlang::has_name(x, "x2")) return(NULL)

  base <- dplyr::tibble(
    code = x[["x2"]],
    a1 = stringfish::sf_substr(code, 1, 1),
    a2 = stringfish::sf_substr(code, 2, 2))

  indices <- base |>
    dplyr::mutate(i1 = dplyr::consecutive_id(a1), .after = code) |>
    dplyr::mutate(i2 = dplyr::consecutive_id(a2), .after = i1, .by = a1)

  orphans <- indices |>
    dplyr::add_count(i1, name = "n1", sort = TRUE) |>
    dplyr::filter((i2 + n1) == 2) |>
    dplyr::pull(code)

  last <- indices |>
    dplyr::filter(!code %in% orphans) |>
    dplyr::select(code, a1:a2) |>
    fuimus::combine(group_id, columns = c("a1"), sep = "")

  last <- dplyr::left_join(
    last|> dplyr::count(group_id, a2),
    last|> dplyr::count(group_id, name = "g"),
    by = dplyr::join_by(group_id)) |>
    dplyr::filter(n == g) |>
    dplyr::right_join(last, by = dplyr::join_by(group_id, a2)) |>
    dplyr::filter(!is.na(n))

  rest <- indices |>
    dplyr::filter(!code %in% c(orphans, dplyr::pull(last, code))) |>
    fuimus::combine(group_id, columns = c("a1"), sep = "")

  grouped <- vctrs::vec_c(
    as.list(orphans),
    vctrs::vec_chop(last$code, sizes = vctrs::vec_run_sizes(last$group_id)),
    vctrs::vec_chop(rest$code, sizes = vctrs::vec_run_sizes(rest$group_id))
  )
  grouped[collapse::radixorderv(collapse::vlengths(grouped), sort = TRUE)]
}
#' @noRd
group_hcpcs_3 <- function(x) {

  if (!rlang::has_name(x, "x3")) return(NULL)

  base <- dplyr::tibble(
    code = x[["x3"]],
    a1 = stringfish::sf_substr(code, 1, 1),
    a2 = stringfish::sf_substr(code, 2, 2),
    a3 = stringfish::sf_substr(code, 3, 3))

  indices <- base |>
    dplyr::mutate(i1 = dplyr::consecutive_id(a1), .after = code) |>
    dplyr::mutate(i2 = dplyr::consecutive_id(a2), .after = i1, .by = a1) |>
    dplyr::mutate(i3 = dplyr::consecutive_id(a3), .after = i2, .by = c(a1, a2))

  orphans <- indices |>
    dplyr::add_count(i1, name = "n1", sort = TRUE) |>
    dplyr::filter((i2 + i3 + n1) == 3) |>
    dplyr::pull(code)

  last <- indices |>
    dplyr::filter(!code %in% orphans) |>
    dplyr::select(code, a1:a3) |>
    fuimus::combine(group_id, columns = c("a1"), sep = "")

  last <- dplyr::left_join(
    last|> dplyr::count(group_id, a2),
    last|> dplyr::count(group_id, name = "g"),
    by = dplyr::join_by(group_id)) |>
    dplyr::filter(n == g) |>
    dplyr::right_join(last, by = dplyr::join_by(group_id, a2)) |>
    dplyr::filter(!is.na(n))

  rest <- indices |>
    dplyr::filter(!code %in% c(orphans, dplyr::pull(last, code))) |>
    fuimus::combine(group_id, columns = c("a1", "a2"), sep = "")

  grouped <- vctrs::vec_c(
    as.list(orphans),
    vctrs::vec_chop(last$code, sizes = vctrs::vec_run_sizes(last$group_id)),
    vctrs::vec_chop(rest$code, sizes = vctrs::vec_run_sizes(rest$group_id))
  )
  grouped[collapse::radixorderv(collapse::vlengths(grouped), sort = TRUE)]
}
#' @noRd
group_hcpcs_4 <- function(x) {

  if (!rlang::has_name(x, "x4")) return(NULL)

  base <- dplyr::tibble(
    code = x[["x4"]],
    a1 = stringfish::sf_substr(code, 1, 1),
    a2 = stringfish::sf_substr(code, 2, 2),
    a3 = stringfish::sf_substr(code, 3, 3),
    a4 = stringfish::sf_substr(code, 4, 4))

  indices <- base |>
    dplyr::mutate(i1 = dplyr::consecutive_id(a1), .after = code) |>
    dplyr::mutate(i2 = dplyr::consecutive_id(a2), .after = i1, .by = a1) |>
    dplyr::mutate(i3 = dplyr::consecutive_id(a3), .after = i2, .by = c(a1, a2)) |>
    dplyr::mutate(i4 = dplyr::consecutive_id(a4), .after = i3, .by = c(a1, a2, a3))

  orphans <- indices |>
    dplyr::add_count(i1, name = "n1", sort = TRUE) |>
    dplyr::filter((i2 + i3 + i4 + n1) == 4) |>
    dplyr::pull(code)

  last <- indices |>
    dplyr::filter(!code %in% orphans) |>
    dplyr::select(code, a1:a4) |>
    fuimus::combine(group_id, columns = c("a1", "a2"), sep = "")

  last <- dplyr::left_join(
    last|> dplyr::count(group_id, a3),
    last|> dplyr::count(group_id, name = "g"),
    by = dplyr::join_by(group_id)) |>
    dplyr::filter(n == g) |>
    dplyr::right_join(last, by = dplyr::join_by(group_id, a3)) |>
    dplyr::filter(!is.na(n))

  rest <- indices |>
    dplyr::filter(!code %in% c(orphans, dplyr::pull(last, code))) |>
    fuimus::combine(group_id, columns = c("a1", "a2", "a3"), sep = "")

  grouped <- vctrs::vec_c(
    as.list(orphans),
    vctrs::vec_chop(last$code, sizes = vctrs::vec_run_sizes(last$group_id)),
    vctrs::vec_chop(rest$code, sizes = vctrs::vec_run_sizes(rest$group_id))
  )
  grouped[collapse::radixorderv(collapse::vlengths(grouped), sort = TRUE)]
}
#' @noRd
group_hcpcs_5 <- function(x) {

  if (!rlang::has_name(x, "x5")) return(NULL)

  base <- dplyr::tibble(
    code = x[["x5"]],
    a1 = stringfish::sf_substr(code, 1, 1),
    a2 = stringfish::sf_substr(code, 2, 2),
    a3 = stringfish::sf_substr(code, 3, 3),
    a4 = stringfish::sf_substr(code, 4, 4),
    a5 = stringfish::sf_substr(code, 5, 5))

  indices <- base |>
    dplyr::mutate(i1 = dplyr::consecutive_id(a1)) |>
    dplyr::mutate(i2 = dplyr::consecutive_id(a2), .by = a1) |>
    dplyr::mutate(i3 = dplyr::consecutive_id(a3), .by = c(a1, a2)) |>
    dplyr::mutate(i4 = dplyr::consecutive_id(a4), .by = c(a1, a2, a3)) |>
    dplyr::mutate(i5 = dplyr::consecutive_id(a5), .by = c(a1, a2, a3, a4)) |>
    dplyr::select(code, a1:a5, i1:i5)

  orphans <- indices |>
    dplyr::add_count(i1, name = "n1", sort = TRUE) |>
    dplyr::filter((i2 + i3 + i4 + i5 + n1) == 5) |>
    dplyr::pull(code)

  last <- indices |>
    dplyr::filter(!code %in% orphans) |>
    dplyr::select(code, a1:a3, a5) |>
    fuimus::combine(group_id, columns = c("a1", "a2", "a3"), sep = "")

  last <- dplyr::left_join(
    last|> dplyr::count(group_id, a5),
    last|> dplyr::count(group_id, name = "g"),
    by = dplyr::join_by(group_id)) |>
    dplyr::filter(n == g) |>
    dplyr::right_join(last,
                      by = dplyr::join_by(group_id, a5)) |>
    dplyr::filter(!is.na(n))

  rest <- indices |>
    dplyr::filter(!code %in% c(orphans, dplyr::pull(last, code))) |>
    fuimus::combine(group_id, columns = c("a1", "a2", "a3", "a4"), sep = "")

  grouped <- vctrs::vec_c(
    as.list(orphans),
    vctrs::vec_chop(last$code, sizes = vctrs::vec_run_sizes(last$group_id)),
    vctrs::vec_chop(rest$code, sizes = vctrs::vec_run_sizes(rest$group_id))
  )
  grouped[collapse::radixorderv(collapse::vlengths(grouped), sort = TRUE)]
}

#' @noRd
process_groups <- function(x) {
  list(
    g1 = group_hcpcs_1(groups),
    g2 = group_hcpcs_2(groups),
    g3 = group_hcpcs_3(groups),
    g4 = group_hcpcs_4(groups),
    g5 = group_hcpcs_5(groups)
  )
}

#' @noRd
pull_orphans  <- function(x) {

  if (is.null(x)) return(NULL)

  x[which(collapse::vlengths(x) == 1)] |> purrr::list_c()

}

#' @noRd
pull_families <- function(x) {

  if (is.null(x)) return(NULL)

  x[which(collapse::vlengths(x) != 1)]

}

# orphans <- list(
#   o2 = pull_orphans(group2),
#   o3 = pull_orphans(group3),
#   o4 = pull_orphans(group4),
#   o5 = pull_orphans(group5))
#
# families <- list(
#   f1 = group_hcpcs_1(groups),
#   f2 = pull_families(group2),
#   f3 = pull_families(group3),
#   f4 = pull_families(group4),
#   f5 = pull_families(group5)
# )

#' @noRd
process_orphans <- function(x) {
  orph <- list(
    o2 = if (!is.null(x$o2)) as.character(glue::glue_collapse(glue::glue('(^{x}[0-9A-Z]{{3}}$)', x = x$o2), sep = "|")) else NULL,
    o3 = if (!is.null(x$o3)) as.character(glue::glue_collapse(glue::glue('(^{x}[0-9A-Z]{{2}}$)', x = x$o3), sep = "|")) else NULL,
    o4 = if (!is.null(x$o4)) as.character(glue::glue_collapse(glue::glue('(^{x}[0-9A-Z]{{1}}$)', x = x$o4), sep = "|")) else NULL,
    o5 = if (!is.null(x$o5)) as.character(glue::glue_collapse(glue::glue('(^{x}$)', x = x$o5), sep = "|")) else NULL
  ) |>
    purrr::compact()

  as.character(glue::glue_collapse(orph, sep = "|"))
}
#' @noRd
process_hcpcs_1 <- function(x) {

  if (is.null(x)) return(NULL)

  re <- fuimus::na_if_common(x) |>
    fuimus::uniq_rmna() |>
    sort_order() |>
    reduce_runs2()

  if (re == "[A-Z0-9]") {

    return(stringr::str_glue("(^{re}{{5}}$)"))

  } else {

    return(stringr::str_glue("(^{re}[A-Z0-9]{{4}}$)"))

  }
}
#' @noRd
process_hcpcs_2 <- function(x) {

  if (is.null(x)) return(NULL)

  vecs <- stringr::str_split_fixed(x, "", n = max(collapse::vlengths(x))) |>
    as.data.frame() |>
    purrr::map(fuimus::na_if_common)

  to_brackets <- vecs |>
    purrr::map(fuimus::uniq_rmna) |>
    purrr::map(sort_order)

  to_vec <- to_brackets |>
    purrr::map(reduce_runs)

  multi_chars <- unname(nchar(to_vec) > 1)

  nobrack <- stringr::str_detect(to_vec[multi_chars], "\\[|\\]", negate = TRUE)

  to_vec[multi_chars] <- if (any(nobrack)) purrr::map_chr(to_vec[multi_chars], \(x) paste0("[", x, "]")) else to_vec[multi_chars]

  to_vec <- purrr::list_c(to_vec)

  fuimus::collapser(to_vec)
}
#' @noRd
process_families <- function(x) {

  x <- list(
    f1 = process_hcpcs_1(families$f1),
    f2 = purrr::map_chr(families$f2, process_hcpcs_2),
    f3 = purrr::map_chr(families$f3, process_hcpcs_2),
    f4 = purrr::map_chr(families$f4, process_hcpcs_2),
    f5 = purrr::map_chr(families$f5, process_hcpcs_2)
  )


  fam <- list(
    f1 = if (!is.null(x$f1)) x$f1 else NULL,
    f2 = if (!is.null(x$f2)) as.character(glue::glue_collapse(glue::glue('(^{x}[0-9A-Z]{{3}}$)', x = x$f2), sep = "|")) else NULL,
    f3 = if (!is.null(x$f3)) as.character(glue::glue_collapse(glue::glue('(^{x}[0-9A-Z]{{2}}$)', x = x$f3), sep = "|")) else NULL,
    f4 = if (!is.null(x$f4)) as.character(glue::glue_collapse(glue::glue('(^{x}[0-9A-Z]{{1}}$)', x = x$f4), sep = "|")) else NULL,
    f5 = if (!is.null(x$f5)) as.character(glue::glue_collapse(glue::glue('(^{x}$)', x = x$f5), sep = "|")) else NULL
  ) |>
    purrr::compact()

  as.character(glue::glue_collapse(fam, sep = "|"))
}
#' @noRd
concatenate_regex <- function(x, y) {
  as.character(glue::glue_collapse(c(x, y), sep = "|"))
}
