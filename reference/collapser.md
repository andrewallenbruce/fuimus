# Wrapper for [`paste0()`](https://rdrr.io/r/base/paste.html) that collapses result

Wrapper for [`paste0()`](https://rdrr.io/r/base/paste.html) that
collapses result

## Usage

``` r
collapser(x)
```

## Arguments

- x:

  A split `<chr>` vector

## Value

A collapsed `<chr>` string

## Examples

``` r
collapser(c("X", "Y", "Z"))
#> [1] "XYZ"
```
