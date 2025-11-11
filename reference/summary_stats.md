# Summary statistics

Summary statistics

## Usage

``` r
summary_stats(
  df,
  condition = NULL,
  group_vars = NULL,
  summary_vars = NULL,
  arr = NULL,
  digits = 3
)
```

## Arguments

- df:

  data frame

- condition:

  filter condition, i.e. `patient == "new"`

- group_vars:

  variables to group by, i.e. `c(specialty, state, hcpcs, cost)`

- summary_vars:

  variables to summarise, i.e. `c(min, max, mode, range)`

- arr:

  column to arrange data by, i.e. `cost`

- digits:

  Number of digits to round to, default is 3

## Value

A `<tibble>` with the summarized data

## Examples

``` r
x <- dplyr::tibble(
   provider = sample(c("A", "B", "C"), size = 200, replace = TRUE),
   city = sample(c("ATL", "NYC"), size = 200, replace = TRUE),
   charges = sample(1000:2000, size = 200),
   payment = sample(1000:2000, size = 200))

summary_stats(
   x,
   condition    = city == "ATL",
   group_vars   = provider,
   summary_vars = c(charges, payment),
   arr          = provider)
#> # A tibble: 3 × 8
#>   provider charges_median charges_mean charges_sd payment_median payment_mean
#>   <chr>             <dbl>        <dbl>      <dbl>          <dbl>        <dbl>
#> 1 C                 1566         1502.       292.          1499         1497.
#> 2 B                 1620.        1581.       281.          1463         1469.
#> 3 A                 1550.        1537.       265.          1530.        1519.
#> # ℹ 2 more variables: payment_sd <dbl>, n <int>
```
