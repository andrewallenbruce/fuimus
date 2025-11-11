# Lagged rate of return

Lagged rate of return

## Usage

``` r
rate_of_return(df, col, n = 1, fill_na = 1L)
```

## Arguments

- df:

  data frame

- col:

  numeric column

- n:

  `<int>` values to offset

- fill_na:

  `<int>` fill value for any NAs; default is 1

## Examples

``` r
mock_provider(2020:2025) |>
  dplyr::group_by(group) |>
  rate_of_return(payment)
#> # A tibble: 12 × 4
#>     year group payment payment_ror
#>    <int> <chr>   <int>       <dbl>
#>  1  2020 A       13775       1    
#>  2  2021 A       16014       1.16 
#>  3  2022 A       11321       0.707
#>  4  2023 A       11887       1.05 
#>  5  2024 A       10005       0.842
#>  6  2025 A       13575       1.36 
#>  7  2020 B       11726       1    
#>  8  2021 B       10315       0.880
#>  9  2022 B       13232       1.28 
#> 10  2023 B       18712       1.41 
#> 11  2024 B       15623       0.835
#> 12  2025 B       12708       0.813
```
