#' Add county, FIPS, and geometry to data frame with zip codes
#'
#' @param df data frame
#'
#' @param state bare column name column containing state abbreviations
#'
#' @param zip bare column name containing zip codes
#'
#' @param fips add county FIPS code column, default is `FALSE`
#'
#' @param geo add county geometry column, default is `FALSE`
#'
#' @param sf convert to an `sf` object, default is `FALSE`
#'
#' @examples
#' (ex <- dplyr::tibble(state = "GA", zip = "31605"))
#'
#' # Add county and latitude/longitude
#' ex |> add_counties(state, zip)
#'
#' # Add county FIPS
#' ex |> add_counties(state, zip, fips = TRUE)
#'
#' # Add county `geometry` column,
#' # based on county FIPS column
#' ex |> add_counties(state, zip, fips = TRUE, geo = TRUE)
#'
#' # Convert to an `sf` object
#' ex |> add_counties(state, zip, fips = TRUE, geo = TRUE, sf = TRUE)
#'
#' @autoglobal
#'
#' @export
add_counties <- function(df, state, zip, fips = FALSE, geo = FALSE, sf = FALSE) {

  by <- dplyr::join_by(
    {{ zip }} == zip,
    {{ state }} == state)

  df <- dplyr::mutate(
    df,
    "{{ zip }}" := zipcodeR::normalize_zip({{ zip }}))

  zdb <- dplyr::tibble(
    zip    = zipcodeR::zip_code_db$zipcode,
    county = gsub(
      " County",
      "",
      zipcodeR::zip_code_db$county,
      fixed = TRUE),
    state  = zipcodeR::zip_code_db$state,
    lat    = zipcodeR::zip_code_db$lat,
    lng    = zipcodeR::zip_code_db$lng)

  df <- dplyr::left_join(df, zdb, by)

  if (fips) {

    pfips <- purrr::possibly(
      fipio::coords_to_fips,
      otherwise = NA_character_)

    df <- dplyr::mutate(
      df,
      county_fips = purrr::map2(
        df$lng,
        df$lat,
        pfips))

    df[apply(
      df,
      2,
      function(x)
        lapply(
          x,
          length
          ) == 0)
      ] <- NA

    df <- tidyr::unnest(
      df,
      county_fips,
      keep_empty = TRUE)
  }

  if (geo) {

    pgeo <- purrr::possibly(
      fipio::fips_geometry,
      otherwise = NA_character_)

    df <- dplyr::mutate(
      df,
      geometry = purrr::map(
        df$county_fips,
        pgeo))
  }

  if (sf) {

    df <- sf::st_as_sf(
      df,
      coords = c("lng", "lat"),
      crs = sf::st_crs(4326),
      na.fail = FALSE)

  }
  return(df)
}

# dplyr::tibble(
#   city = c('Mount Zion', 'Montezuma', 'Thomaston'),
#   state = fct_stabb(rep('GA', 3)),
#   county = c('Carroll', 'Macon', 'Upson'),
#   zip = c('30150', '31063', '30286'),
#   county_fips = c('13045', '13193', '13293')
# )
