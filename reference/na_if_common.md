# Convert various character strings to `NA`

Convert various character strings to `NA`

## Usage

``` r
na_if_common(x)
```

## Arguments

- x:

  `<chr>` vector to convert

## Value

`<chr>` vector with converted `NA` values

## Examples

``` r
na_if_common(x = c(" ", "*", "--", "N/A", "", "A", "B"))
#> [1] NA  NA  NA  NA  NA  "A" "B"
```
