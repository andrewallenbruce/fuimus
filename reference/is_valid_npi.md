# Validate NPIs (v1)

Validate NPIs (v1)

## Usage

``` r
is_valid_npi(npi)
```

## Arguments

- npi:

  `<character>` Number of NPIs to generate

## Value

`<logical>` vector

## Examples

``` r
is_valid_npi(1043477615)
#> [1] TRUE

is_valid_npi(1234567891)
#> [1] FALSE

x <- random_npi_generator(100)

x[collapse::whichv(purrr::map_lgl(x, is_valid_npi), TRUE)]
#> [1] "1210558510" "1135120602" "2160772713" "1141874317" "1337019487"
#> [6] "1517376491" "2767083753" "1306015185" "1226192619"
```
