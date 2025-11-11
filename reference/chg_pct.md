# Lagged percentage change

Lagged percentage change

## Usage

``` r
chg_pct(x, n = 1L, fill_na = 0L)
```

## Arguments

- x:

  numeric vector

- n:

  values to offset

- fill_na:

  fill value for any NAs; default is 0

## Examples

``` r
mock_provider(2020:2025) |>
  dplyr::mutate(change = chg_pct(payment), .by = group)
#> # A tibble: 12 × 4
#>     year group payment change
#>    <int> <chr>   <int>  <dbl>
#>  1  2020 A       16595  0    
#>  2  2021 A       10745 -0.353
#>  3  2022 A       12624  0.175
#>  4  2023 A       19877  0.575
#>  5  2024 A       13841 -0.304
#>  6  2025 A       19767  0.428
#>  7  2020 B       11216  0    
#>  8  2021 B       16299  0.453
#>  9  2022 B       13083 -0.197
#> 10  2023 B       15557  0.189
#> 11  2024 B       11832 -0.239
#> 12  2025 B       13649  0.154
```
