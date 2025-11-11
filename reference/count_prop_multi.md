# Count and calculate proportion of each group

Count and calculate proportion of each group

## Usage

``` r
count_prop_multi(df, rows, cols, sort = FALSE, na.rm = FALSE)
```

## Arguments

- df:

  A `<data.frame>` or `<tibble>`

- rows:

  A `<character>` or `<symbol>` specifying the rows to count

- cols:

  A `<character>` or `<symbol>` specifying the columns to count

- sort:

  A `<logical>` indicating whether to sort the output by frequency,
  default is `FALSE`

- na.rm:

  A `<logical>` indicating whether to remove missing values from the
  count, default is `FALSE`

## Examples

``` r
mock_forager(10) |>
  count_prop_multi(
  c(class),
  payer,
  sort = TRUE)
#> # A tibble: 10 × 4
#>    class     payer        n  prop
#>    <ord>     <fct>    <int> <dbl>
#>  1 Primary   BCBS         1   0.1
#>  2 Primary   Centene      1   0.1
#>  3 Primary   Cigna        1   0.1
#>  4 Primary   Medicare     1   0.1
#>  5 Secondary Anthem       1   0.1
#>  6 Secondary Cigna        1   0.1
#>  7 Secondary Humana       1   0.1
#>  8 Secondary MAO          1   0.1
#>  9 Secondary Medicaid     1   0.1
#> 10 Secondary UHC          1   0.1
```
