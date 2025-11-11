# Plots a fancy time series

Plots a fancy time series

## Usage

``` r
fancy_ts(df, val, group)
```

## Arguments

- df:

  data.frame

- val:

  value var

- group:

  group var

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
with the summarized data

## Examples

``` r
df <- dplyr::tibble(
   dist1 = sort(rnorm(50, 5, 2)),
   dist2 = sort(rnorm(50, 8, 3)),
   dist4 = sort(rnorm(50, 15, 1)),
   date = seq.Date(
      as.Date("2022-01-01"),
      as.Date("2022-04-10"),
      by = "2 days")) |>
tidyr::pivot_longer(
   cols = -date,
   names_to = "dist_name",
   values_to = "value")

df
#> # A tibble: 150 × 3
#>    date       dist_name  value
#>    <date>     <chr>      <dbl>
#>  1 2022-01-01 dist1     -0.172
#>  2 2022-01-01 dist2      0.389
#>  3 2022-01-01 dist4     12.0  
#>  4 2022-01-03 dist1      0.642
#>  5 2022-01-03 dist2      1.95 
#>  6 2022-01-03 dist4     12.6  
#>  7 2022-01-05 dist1      0.943
#>  8 2022-01-05 dist2      2.23 
#>  9 2022-01-05 dist4     13.2  
#> 10 2022-01-07 dist1      1.25 
#> # ℹ 140 more rows

fancy_ts(df, value, dist_name) +
gg_theme()
#> Warning: The `size` argument of `element_line()` is deprecated as of ggplot2 3.4.0.
#> ℹ Please use the `linewidth` argument instead.
#> ℹ The deprecated feature was likely used in the fuimus package.
#>   Please report the issue at
#>   <https://github.com/andrewallenbruce/fuimus/issues>.

```
