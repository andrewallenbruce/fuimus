# Sorted Bar Chart

Sorted Bar Chart

## Usage

``` r
sorted_bars(df, var)
```

## Arguments

- df:

  data frame

- var:

  column to plot

## Value

A
[ggplot2](https://ggplot2.tidyverse.org/reference/ggplot2-package.html)
object

## Examples

``` r
ggplot2::diamonds |>
  sorted_bars(cut) +
  gg_theme()

```
