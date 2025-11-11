# Calculate lagged values by column

Calculate lagged values by column

## Usage

``` r
change_lagged(df, col, by = NULL)
```

## Arguments

- df:

  data frame

- col:

  column of numeric values to calculate lag

- by:

  column to calculate lag by

## Examples

``` r
mock_provider(2020:2025) |>
  dplyr::group_by(group) |>
  change_lagged(payment, year)
#> # A tibble: 12 × 5
#> # Groups:   group [2]
#>     year group payment payment_chg payment_pct
#>    <int> <chr>   <int>       <int>       <dbl>
#>  1  2020 A       18943          NA     NA     
#>  2  2021 A       19285         342      0.0181
#>  3  2022 A       16293       -2992     -0.155 
#>  4  2023 A       11022       -5271     -0.324 
#>  5  2024 A       11289         267      0.0242
#>  6  2025 A       19121        7832      0.694 
#>  7  2020 B       14275          NA     NA     
#>  8  2021 B       12069       -2206     -0.155 
#>  9  2022 B       11920        -149     -0.0123
#> 10  2023 B       14274        2354      0.197 
#> 11  2024 B       16829        2555      0.179 
#> 12  2025 B       14928       -1901     -0.113 
```
