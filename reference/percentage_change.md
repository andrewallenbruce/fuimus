# Calculate the percentage change between two values

Percentage change is obtained by dividing the change in value by the
original value.

## Usage

``` r
percentage_change(new, old)
```

## Arguments

- new:

  `<dbl>` newer value

- old:

  `<dbl>` older value

## Value

`<dbl>` vector of percentage change

## Details

If the result is positive, then it is an increase. If the result is
negative, then it is a decrease.

## Examples

``` r
percentage_change(new = 949740, old = 942482)
#> [1] 0.007700943

percentage_change(new = 942482, old = 1132783)
#> [1] -0.1679942
```
