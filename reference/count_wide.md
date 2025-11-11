# Count rows and columns and pivot to wide format

Count rows and columns and pivot to wide format

## Usage

``` r
count_wide(df, rows, cols)
```

## Arguments

- df:

  A `<data.frame>` or `<tibble>`

- rows:

  A `<character>` or `<symbol>` specifying the rows to count

- cols:

  A `<character>` or `<symbol>` specifying the columns to count

## Examples

``` r
mock_forager(10) |>
  count_wide(c(class), payer)
#> # A tibble: 2 × 6
#>   class      BCBS Centene Humana   MAO Medicaid
#>   <ord>     <int>   <int>  <int> <int>    <int>
#> 1 Primary       0       1      1     1        3
#> 2 Secondary     1       2      1     0        0
```
