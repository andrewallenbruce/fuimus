# Validate NPIs (v2)

Validate NPIs (v2)

## Usage

``` r
is_valid_npi2(x)
```

## Arguments

- x:

  `<character>` Number of NPIs to generate

## Value

`<logical>` vector

## Examples

``` r
is_valid_npi2("1043477615")
#> [1] TRUE

is_valid_npi2("1234567891")
#> [1] FALSE

x <- random_npi_generator(100)

x[collapse::whichv(purrr::map_lgl(x, is_valid_npi2), TRUE)]
#>  [1] "2334551795" "1781864982" "2418777795" "1342443367" "2360667788"
#>  [6] "1796226375" "1243122607" "1305682175" "1443974005" "1004917716"
```
