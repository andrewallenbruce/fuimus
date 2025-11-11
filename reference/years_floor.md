# Calculate the number of years between two dates rounded down to the nearest whole number

Calculate the number of years between two dates rounded down to the
nearest whole number

## Usage

``` r
years_floor(from, to)
```

## Arguments

- from:

  `<date>` Start date

- to:

  `<date>` End date

## Value

`<dbl>` vector of the number of years between the two dates rounded down
to the nearest whole number

## Examples

``` r
years_floor(
   from = as.Date("2020-01-01"),
   to = as.Date("2020-01-01") + 2057)
#> [1] 5
```
