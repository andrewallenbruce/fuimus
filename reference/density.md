# Density plot

Density plot

## Usage

``` r
density(df, var, color, facets, binwidth = 0.1)
```

## Arguments

- df:

  df

- var:

  var

- color:

  color var

- facets:

  facets var

- binwidth:

  binwidth

## Value

ggplot2 density plot

## Examples

``` r
ggplot2::diamonds |> density(carat)


ggplot2::diamonds |> density(carat, cut)


ggplot2::diamonds |> density(carat, cut, clarity)

```
