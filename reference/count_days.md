# Count days between two dates

Count days between two dates

## Usage

``` r
count_days(df, start, end, name)
```

## Arguments

- df:

  A `<data.frame>` or `<tibble>`

- start:

  `<sym>` bare date column name

- end:

  `<sym>` bare date column name

- name:

  `<sym>` bare name of days output column

## Value

`<data.frame>` or `<tibble>`

## Examples

``` r
dplyr::tibble(dos    = as.Date(c("2021-04-18", "2021-11-18", "2022-02-18")),
              signed = as.Date("2022-02-18")) |>
              count_days(
              start  = dos,
              end    = signed,
              lag)
#> # A tibble: 3 × 3
#>   dos        signed       lag
#>   <date>     <date>     <int>
#> 1 2021-04-18 2022-02-18   306
#> 2 2021-11-18 2022-02-18    92
#> 3 2022-02-18 2022-02-18     0
```
