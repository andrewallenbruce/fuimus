# Infix `if (!is.null(x)) y else x`

Infix `if (!is.null(x)) y else x`

## Usage

``` r
x %nn% y
```

## Arguments

- x, y:

  vectors

## Value

`y` if `x` is not `NULL`, else `x`

## Examples

``` r
NULL %nn% 123456L
#> NULL

"abc" %nn% 123456L
#> [1] 123456
```
