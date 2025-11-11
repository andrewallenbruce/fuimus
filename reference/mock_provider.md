# Generate simple data frame for testing

Generate simple data frame for testing

## Usage

``` r
mock_provider(years)
```

## Arguments

- years:

  `<int>` vector (`c(2020, 2021, 2023)`) or sequence of years
  (`2010:2020`)

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
containing the search results.

## See also

Other mock:
[`mock_forager()`](https://andrewallenbruce.github.io/fuimus/reference/mock_forager.md),
[`random_npi_generator()`](https://andrewallenbruce.github.io/fuimus/reference/random_npi_generator.md)

## Examples

``` r
mock_provider(2010:2020)
#> # A tibble: 22 × 3
#>     year group payment
#>    <int> <chr>   <int>
#>  1  2010 A       15771
#>  2  2011 A       12756
#>  3  2012 A       16711
#>  4  2013 A       17571
#>  5  2014 A       16131
#>  6  2015 A       14033
#>  7  2016 A       16766
#>  8  2017 A       11915
#>  9  2018 A       14965
#> 10  2019 A       16986
#> # ℹ 12 more rows

mock_provider(c(2020, 2021, 2023))
#> # A tibble: 6 × 3
#>    year group payment
#>   <dbl> <chr>   <int>
#> 1  2020 A       11750
#> 2  2021 A       14150
#> 3  2023 A       15116
#> 4  2020 B       10159
#> 5  2021 B       10483
#> 6  2023 B       17461
```
