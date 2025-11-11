# Calculate the new value from the old value and the percentage change between the two values

Calculate the new value from the old value and the percentage change
between the two values

## Usage

``` r
new_value(old, percentage_change)
```

## Arguments

- old:

  `<dbl>` older value

- percentage_change:

  `<dbl>` percentage change

## Value

`<dbl>` vector of new value

## Examples

``` r
new_value(old = 942482, percentage_change = 0.007700943)
#> [1] 949740

new_value(old = 1132783, percentage_change = -0.1679942)
#> [1] 942482

# If the value of an object increased by 220%,
# from the original value of
# $500,000, what is it worth now?

new_value(old = 500000, percentage_change = 2.2)
#> [1] 1600000
```
