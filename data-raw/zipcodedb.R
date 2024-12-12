source(here::here("data-raw", "pins_internal.R"))

# zipcodes <- zipcodeR::zip_code_db

zipcodes <- get_pin("zipcodes") |>
  dplyr::reframe(
    zip    = zipcode,
    county = gsub(" County", "", county, fixed = TRUE),
    state  = state,
    lat    = lat,
    lng    = lng)

# TOOK ABOUT 15 MINUTES TO RUN:
zipcodes[["fips"]] <- fipio::coords_to_fips(zipcodes, coords = c("lng", "lat"))

zipcodes[zipcodes$fips == "", ] <- NA_character_

# SHOULD'VE BEEN:
# zipcodes$fips[zipcodes$fips == "", ] <- NA_character_

zipcodes <- zipcodes[not_na(zipcodes$fips), ]

fips_geo <- dplyr::tibble(
  fips = uniq(zipcodes$fips),
  geometry = fipio::fips_geometry(fips))

pin_update(
  fips_geo,
  name = "fips_geo",
  title = "FIPS Code to Spatial Geometry",
  description = "Subset of Zip Code Database")

coords_sf <- sf::st_as_sf(
  zipcodes,
  coords = c("lng", "lat"),
  crs = sf::st_crs(4326),
  na.fail = FALSE)

pin_update(
  coords_sf,
  name = "coords_sf",
  title = "Lat-Long to SF Object",
  description = "Lat-Long to SF Object")

zip_subset <- dplyr::tibble(zipcodes)

pin_update(
  zip_subset,
  name = "zip_subset",
  title = "Subset of Zip Code Database",
  description = "Subset of Zip Code Database")
