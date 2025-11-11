# Pad numbers with zeroes

Pad numbers with zeroes

## Usage

``` r
pad_number(x, digits = NULL)
```

## Arguments

- x:

  `<dbl>` vector of numbers

- digits:

  `<int>` single integer; the number of zeroes to pad `x` with. If left
  `NULL` (the default), will use the result of `max(nchar(x))`

## Value

`<chr>` vector of padded numbers

## Examples

``` r
pad_number(c(56, 2584, 010, 912, 12222))
#> [1] "00056" "02584" "00010" "00912" "12222"

pad_number(c(56, 2584, 010, 912, 00012222), digits = 10L)
#> [1] "0000000056" "0000002584" "0000000010" "0000000912" "0000012222"
```
