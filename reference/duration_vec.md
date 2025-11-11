# Calculate the duration between two dates

Calculate the duration between two dates

## Usage

``` r
duration_vec(from, to)
```

## Arguments

- from:

  `<date>` Start date

- to:

  `<date>` End date

## Value

`<dbl>` vector of the duration between the two dates

## Examples

``` r
duration_vec(
  from = lubridate::today() - 366,
  to = lubridate::today())
#> [1] "31622400s (~1 years)"
```
