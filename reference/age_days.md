# Calculate Number of Days Between Two Dates

Calculate Number of Days Between Two Dates

## Usage

``` r
age_days(df, start, end, colname = "age")
```

## Arguments

- df:

  `<data.frame>` containing date columns

- start:

  column containing date(s) prior to end_date column

- end:

  column containing date(s) after start_date column

- colname:

  desired column name of output; default is "age"

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble-package.html)
with a named column containing the calculated number of days.

## Note

This calculation includes the end date in the sum (see example)

## Examples

``` r
date_ex <- dplyr::tibble(
   x = seq.Date(
   as.Date("2021-01-01"),
   by = "month",
   length.out = 3),
   y = seq.Date(
   as.Date("2022-01-01"),
   by = "month",
   length.out = 3
   )
 )

age_days(df    = date_ex,
         start = x,
         end   = y)
#> # A tibble: 3 × 3
#>   x          y            age
#>   <date>     <date>     <dbl>
#> 1 2021-01-01 2022-01-01   366
#> 2 2021-02-01 2022-02-01   366
#> 3 2021-03-01 2022-03-01   366

date_ex |>
age_days(x, y, "days_between_x_y")
#> # A tibble: 3 × 3
#>   x          y          days_between_x_y
#>   <date>     <date>                <dbl>
#> 1 2021-01-01 2022-01-01              366
#> 2 2021-02-01 2022-02-01              366
#> 3 2021-03-01 2022-03-01              366

date_ex |>
age_days(start = x,
end = lubridate::today(),
colname = "days_since_x")
#> # A tibble: 3 × 3
#>   x          y          days_since_x
#>   <date>     <date>            <dbl>
#> 1 2021-01-01 2022-01-01         1776
#> 2 2021-02-01 2022-02-01         1745
#> 3 2021-03-01 2022-03-01         1717

date_ex |>
age_days(x, y, "days_between_x_y") |>
age_days(x, lubridate::today(), "days_since_x") |>
age_days(y, lubridate::today(), colname = "days_since_y")
#> # A tibble: 3 × 5
#>   x          y          days_between_x_y days_since_x days_since_y
#>   <date>     <date>                <dbl>        <dbl>        <dbl>
#> 1 2021-01-01 2022-01-01              366         1776         1411
#> 2 2021-02-01 2022-02-01              366         1745         1380
#> 3 2021-03-01 2022-03-01              366         1717         1352
```
