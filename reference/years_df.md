# Calculate the number of years between two dates

Calculate the number of years between two dates

## Usage

``` r
years_df(df, from, to)
```

## Arguments

- df:

  A `<data.frame>` or `<tibble>`

- from:

  `<date>` Start date column

- to:

  `<date>` End date column

## Value

`<data.frame>` or `<tibble>`

## Examples

``` r
dplyr::tibble(
  from = as.Date("2020-01-01"),
  to = as.Date("2020-01-01") + 2057) |>
  years_df(from, to)
#> # A tibble: 1 × 3
#>   from       to         years_passed
#>   <date>     <date>            <dbl>
#> 1 2020-01-01 2025-08-19         5.63
```
