# Linearly interpolate values between two points

[SO
Link](https://stackoverflow.com/questions/27920690/linear-interpolation-using-dplyr/31845035#31845035)

## Usage

``` r
interpolate(x, y, method = c("approx", "spline"))
```

## Arguments

- x, y:

  `<numeric>` vectors giving the coordinates of the points to be
  interpolated

- method:

  `<character>` interpolation method, default is `"approx"`

## Value

`<numeric>` vector of interpolated values

## See also

Other base:
[`colon()`](https://andrewallenbruce.github.io/fuimus/reference/colon.md)

## Examples

``` r
interpolate(1:5, c(10, NA, NA, NA, 100), "spline")
#> [1]  10.0  32.5  55.0  77.5 100.0

df <- dplyr::tibble(
  seq = 1:5,
  v1 = c(1, NA, 3, NA, 5),
  v2 = c(40, NA, 60, NA, 70),
  v3 = c(10, NA, NA, NA, 100))

df
#> # A tibble: 5 × 4
#>     seq    v1    v2    v3
#>   <int> <dbl> <dbl> <dbl>
#> 1     1     1    40    10
#> 2     2    NA    NA    NA
#> 3     3     3    60    NA
#> 4     4    NA    NA    NA
#> 5     5     5    70   100

df |>
   dplyr::mutate(
   dplyr::across(
   .cols = dplyr::starts_with("v"),
   .fns = ~ interpolate(seq, .x),
   .names = "{.col}_est"))
#> # A tibble: 5 × 7
#>     seq    v1    v2    v3 v1_est v2_est v3_est
#>   <int> <dbl> <dbl> <dbl>  <dbl>  <dbl>  <dbl>
#> 1     1     1    40    10      1     40   10  
#> 2     2    NA    NA    NA      2     50   32.5
#> 3     3     3    60    NA      3     60   55  
#> 4     4    NA    NA    NA      4     65   77.5
#> 5     5     5    70   100      5     70  100  

df |>
  dplyr::mutate(
   dplyr::across(
    dplyr::starts_with("v"),
     ~ interpolate(seq, .x)))
#> # A tibble: 5 × 4
#>     seq    v1    v2    v3
#>   <int> <dbl> <dbl> <dbl>
#> 1     1     1    40  10  
#> 2     2     2    50  32.5
#> 3     3     3    60  55  
#> 4     4     4    65  77.5
#> 5     5     5    70 100  
```
