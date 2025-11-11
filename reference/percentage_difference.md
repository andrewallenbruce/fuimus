# Calculate the percentage difference between two values

Percentage difference is obtained by dividing the absolute value of
change by the average of the values and then multiplying it with 100.

## Usage

``` r
percentage_difference(x, y)
```

## Arguments

- x, y:

  `<dbl>` values to determine the percent difference between, order does
  not matter

## Value

`<dbl>` vector of percentage difference

## Details

Since the absolute value is taken for the change (or difference) in
values, the order of the numbers does not matter.

## Examples

``` r
percentage_difference(949740, 942482)
#> [1] 0.007671404

percentage_difference(942482, 1132783)
#> [1] 0.1833992
```
