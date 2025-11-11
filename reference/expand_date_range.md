# Expand date range to include all dates

Expand date range to include all dates

## Usage

``` r
expand_date_range(x, name)
```

## Arguments

- x:

  `<date>` vector of dates

- name:

  `<chr>` new column name for vector of dates

## Value

`<tbl>` with expanded date range

## Examples

``` r
expand_date_range(
   seq.Date(
   from = as.Date("2020-01-01"),
   to = as.Date("2020-01-01") + 2057,
   by = 1),
   "date"
)
#> Error in check_is_df(data): `x` must be a data.frame
```
