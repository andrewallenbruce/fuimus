# Generate mock coding/billing data frame

Generate mock coding/billing data frame

## Usage

``` r
mock_forager(rows = 10, nest = FALSE)
```

## Arguments

- rows:

  `<int>` number of rows to generate; default is 10

- nest:

  `<lgl>` whether to nest the dates column; default is `FALSE`

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)

## See also

Other mock:
[`mock_provider()`](https://andrewallenbruce.github.io/fuimus/reference/mock_provider.md),
[`random_npi_generator()`](https://andrewallenbruce.github.io/fuimus/reference/random_npi_generator.md)

## Examples

``` r
mock_forager(rows = 5)
#> # A tibble: 5 × 9
#>   id    payer  class  balance date_of_service date_of_release date_of_submission
#>   <chr> <fct>  <ord>    <dbl> <date>          <date>          <date>            
#> 1 0001  Cigna  Secon…   163.  2025-01-11      2025-01-28      2025-01-29        
#> 2 0002  MAO    Secon…    16.1 2025-07-11      2025-07-23      2025-07-24        
#> 3 0003  Cigna  Secon…   191.  2025-09-11      2025-09-23      2025-09-25        
#> 4 0004  Humana Secon…    91.9 2024-12-11      2024-12-14      2024-12-20        
#> 5 0005  UHC    Prima…   102.  2025-05-11      2025-05-22      2025-05-24        
#> # ℹ 2 more variables: date_of_acceptance <date>, date_of_adjudication <date>
```
