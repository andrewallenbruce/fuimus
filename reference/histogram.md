# Histogram

Histogram

## Usage

``` r
histogram(df, var, binwidth)
```

## Arguments

- df:

  data frame

- var:

  var

- binwidth:

  `<numeric>` binwidth

## Value

ggplot2 histogram

## Examples

``` r
ggplot2::diamonds |>
  histogram(carat, 0.1)

```
