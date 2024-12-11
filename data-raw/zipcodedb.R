source(here::here("data-raw", "pins_internal.R"))

zipcodes <- zipcodeR::zip_code_db

pin_update(
  zipcodes,
  name = "zipcodes",
  title = "Zip Code Database from zipcodeR package",
  description = "https://github.com/MacHu-GWU/uszipcode-project/files/5183256/simple_db.log"
)
