# Create Integer Sequence Beginning at 1

Create Integer Sequence Beginning at 1

## Usage

``` r
colon(n)
```

## Arguments

- n:

  `<integer>` Ending number of sequence

## Value

`<integer>` vector of numbers from 1 to `n`

## See also

Other base:
[`interpolate()`](https://andrewallenbruce.github.io/fuimus/reference/interpolate.md)

## Examples

``` r
colon(50)
#>  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
#> [26] 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50

colon(-20)
#>  [1]   1   0  -1  -2  -3  -4  -5  -6  -7  -8  -9 -10 -11 -12 -13 -14 -15 -16 -17
#> [20] -18 -19 -20

colon(0 + 150 - 145)
#> [1] 1 2 3 4 5

colon(20.9)
#> Warning: `n` has been coerced to `<integer>`.
#>  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20

colon(20.1)
#> Warning: `n` has been coerced to `<integer>`.
#>  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
```
