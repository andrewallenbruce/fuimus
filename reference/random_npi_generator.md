# Generate random NPIs

Generate random NPIs

## Usage

``` r
random_npi_generator(n)
```

## Arguments

- n:

  `<integer>` Number of NPIs to generate

## Value

`<character>` vector of `n` NPIs

## See also

Other mock:
[`mock_forager()`](https://andrewallenbruce.github.io/fuimus/reference/mock_forager.md),
[`mock_provider()`](https://andrewallenbruce.github.io/fuimus/reference/mock_provider.md)

## Examples

``` r
random_npi_generator(10)
#>  [1] "1516929829" "2868387063" "2028957361" "1731350685" "1209497160"
#>  [6] "2220098487" "2641859660" "1565771964" "2297874122" "1375054749"

x <- random_npi_generator(100)

x[collapse::whichv(purrr::map_lgl(x, is_valid_npi), TRUE)]
#> [1] "2199368574" "1042288948" "1318886078" "2162607651" "1814750372"
#> [6] "1710586680"
```
