# Calculate the number of years between two dates

Calculate the number of years between two dates

## Usage

``` r
years_vec(from, to)
```

## Arguments

- from:

  `<date>` Start date

- to:

  `<date>` End date

## Value

`<dbl>` vector of the number of years between the two dates

## Examples

``` r
years_vec(
  from = as.Date("2020-01-01"),
  to = as.Date("2020-01-01") + 2057)
#> [1] 5.63
```
