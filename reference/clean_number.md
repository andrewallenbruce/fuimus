# Clean character vector of numbers

Clean character vector of numbers

## Usage

``` r
clean_number(x)
```

## Arguments

- x:

  `<chr>` vector of numbers

## Value

a `<dbl>` vector of numbers

## Examples

``` r
clean_number(c("20%", "21,125,458", "$123"))
#> [1]        0.2 21125458.0      123.0
```
