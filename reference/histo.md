# Inline histogram

Inline histogram

## Usage

``` r
histo(x, width = 10)
```

## Arguments

- x:

  description

- width:

  description

## Value

description

## Examples

``` r
histo(x = rnorm(100))
#> [1] "▁▁▃▅▅▇▂▂▁"

histo(x = runif(100))
#> [1] "▃▇▇▃▃▅▇▂▇▅"

histo(x = rnorm(100), width = 5)
#> [1] "▁▃▅▇▃▁"

histo(x = runif(100), width = 5)
#> [1] "▅▇▅▇▇"
```
