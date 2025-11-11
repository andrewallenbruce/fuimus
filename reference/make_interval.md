# Create interval, period and time length columns from a start and end date

Create interval, period and time length columns from a start and end
date

## Usage

``` r
make_interval(df, start, end = lubridate::today())
```

## Arguments

- df:

  A `<data.frame>` or `<tibble>`

- start:

  `<date>` Start date column

- end:

  `<date>` End date column

## Value

`<data.frame>` or `<tibble>`

## Examples

``` r
dplyr::tibble(date = lubridate::today() - 366) |>
  make_interval(start = date)
#> # A tibble: 1 × 4
#>   date       interval                       period            timelength_days
#>   <date>     <Interval>                     <Period>                    <dbl>
#> 1 2024-11-10 2024-11-10 UTC--2025-11-11 UTC 1y 0m 1d 0H 0M 0S             366
```
