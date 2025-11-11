# Count and calculate proportion of each group

Count and calculate proportion of each group

## Usage

``` r
count_prop(df, var, sort = FALSE, na.rm = FALSE)
```

## Arguments

- df:

  A `<data.frame>` or `<tibble>`

- var:

  A `<character>` or `<symbol>` specifying the column to count

- sort:

  A `<logical>` indicating whether to sort the output by frequency,
  default is `FALSE`

- na.rm:

  A `<logical>` indicating whether to remove missing values from the
  count, default is `FALSE`

## Examples

``` r
mock_forager() |>
  count_prop(payer, sort = TRUE)
#> # A tibble: 7 × 3
#>   payer        n  prop
#>   <fct>    <int> <dbl>
#> 1 Medicare     3   0.3
#> 2 Cigna        2   0.2
#> 3 BCBS         1   0.1
#> 4 Centene      1   0.1
#> 5 Humana       1   0.1
#> 6 MAO          1   0.1
#> 7 Medicaid     1   0.1
```
