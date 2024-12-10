#' Chop Vector by Group
#'
#' @param v `<character>` vector
#'
#' @param g `<integer>` group
#'
#' @returns `<list>`
#'
#' @examples
#' (v <- c("222", "280", "3020", "8690", "G0294", "G8126"))
#'
#' (g <- sample(1:2, size = length(v), replace = TRUE))
#'
#' gchop(v, g)
#'
#' @autoglobal
#'
#' @family vctrs
#'
#' @export
gchop <- \(v, g) vctrs::vec_chop(x = v, sizes = vctrs::vec_run_sizes(g))
