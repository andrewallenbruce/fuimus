# Percentage calculator

If *a* is some *p%* of *b*, then:

- *p* = a/b

- *a* = p \* b

- *b* = a/p

## Usage

``` r
percentage_calculator(a, b)
```

## Arguments

- a, b:

  `<dbl>` values

## Value

`<dbl>` vector of percentage difference

## Details

Since the absolute value is taken for the change (or difference) in
values, the order of the numbers does not matter.

## Examples

``` r
percentage_calculator(2500, 133)
#> $p
#> [1] 18.79699
#> 
#> $a
#> [1] 2500
#> 
#> $b
#> [1] 133
#> 

percentage_calculator(133, 2500)
#> $p
#> [1] 0.0532
#> 
#> $a
#> [1] 133
#> 
#> $b
#> [1] 2500
#> 
```
