# Wrapper for [`unlist()`](https://rdrr.io/r/base/unlist.html), with `use.names` set to `FALSE`

Wrapper for [`unlist()`](https://rdrr.io/r/base/unlist.html), with
`use.names` set to `FALSE`

## Usage

``` r
delister(x)
```

## Arguments

- x:

  A named `<list>`

## Value

An unnamed `<chr>` vector

## Examples

``` r
delister(list(x = "XYZ"))
#> [1] "XYZ"
```
