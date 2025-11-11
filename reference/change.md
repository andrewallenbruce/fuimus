# Calculate Lagged Metrics

Calculate Lagged Metrics

## Usage

``` r
change(df, cols, csm = NULL)
```

## Arguments

- df:

  data frame

- cols:

  numeric columns to calculate absolute/relative change & rate of return

- csm:

  numeric cols to calculate cumulative sum for

## Examples

``` r
mock_provider(2020:2025) |>
  dplyr::group_by(group) |>
  change(payment, csm = c("payment", "_chg"))
#> # A tibble: 12 × 8
#>     year group payment payment_chg payment_pct payment_ror payment_csm
#>    <int> <chr>   <int>       <int>       <dbl>       <dbl>       <int>
#>  1  2020 A       12606           0     0             1           12606
#>  2  2021 A       11475       -1131    -0.0897        0.910       24081
#>  3  2022 A       18165        6690     0.583         1.58        42246
#>  4  2023 A       12244       -5921    -0.326         0.674       54490
#>  5  2024 A       14612        2368     0.193         1.19        69102
#>  6  2025 A       19982        5370     0.368         1.37        89084
#>  7  2020 B       14172           0     0             1           14172
#>  8  2021 B       19089        4917     0.347         1.35        33261
#>  9  2022 B       12614       -6475    -0.339         0.661       45875
#> 10  2023 B       13371         757     0.0600        1.06        59246
#> 11  2024 B       11980       -1391    -0.104         0.896       71226
#> 12  2025 B       12046          66     0.00551       1.01        83272
#> # ℹ 1 more variable: payment_chg_csm <int>
```
