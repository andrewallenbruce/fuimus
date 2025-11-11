# Lagged absolute change

Lagged absolute change

## Usage

``` r
chg_abs(x, n = 1L, fill_na = 0L)
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
  dplyr::mutate(change = chg_abs(payment), .by = group)
#> # A tibble: 12 × 4
#>     year group payment change
#>    <int> <chr>   <int>  <int>
#>  1  2020 A       15617      0
#>  2  2021 A       16308    691
#>  3  2022 A       12783  -3525
#>  4  2023 A       12923    140
#>  5  2024 A       11874  -1049
#>  6  2025 A       17963   6089
#>  7  2020 B       11839      0
#>  8  2021 B       19177   7338
#>  9  2022 B       11314  -7863
#> 10  2023 B       18321   7007
#> 11  2024 B       13926  -4395
#> 12  2025 B       17017   3091
```
