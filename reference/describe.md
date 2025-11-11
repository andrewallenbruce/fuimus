# Describe a dataset

Describe a dataset

## Usage

``` r
describe(df, ...)
```

## Arguments

- df:

  `<data.frame>` desc

- ...:

  `<dots>` tidyselect columns

## Value

`<tibble>` of summary statistics

## Examples

``` r
describe(mock_provider(2000:2020))
#> # A tibble: 3 × 12
#>   variable type  range   histogram   mean     sd   iqr    med    mad     n nuniq
#>   <chr>    <fct> <chr>   <chr>      <dbl>  <dbl> <dbl>  <dbl>  <dbl> <int> <int>
#> 1 group    <chr> [1 - 1] ▇             1  0          0     1  0         42     2
#> 2 year     <int> [2000 … ▇▅▅▅▅▅▅▅…  2010  6.13e0    10  2010  7.41e0    42    21
#> 3 payment  <int> [10103… ▇▅▅▂▇▁▅▃… 14117. 2.72e3  4307 14146. 3.18e3    42    42
#> # ℹ 1 more variable: top_n <chr>

describe(mock_forager(200), !dplyr::starts_with("date"))
#> # A tibble: 4 × 12
#>   variable type  range     histogram   mean     sd   iqr   med   mad     n nuniq
#>   <chr>    <fct> <chr>     <chr>      <dbl>  <dbl> <dbl> <dbl> <dbl> <int> <int>
#> 1 balance  <dbl> [3.59 - … ▅▇▇▃▂▁▁▁… 124.   88.5    107.  110. 79.4    200   200
#> 2 class    <ord> [1 - 2]   ▇▁▁▁▁▁▁▁…   1.52  0.501    1     2   0      200     2
#> 3 payer    <fct> [1 - 9]   ▇▃▃▃▃▂▃▃    4.87  2.65     4     5   2.97   200     9
#> 4 id       <chr> [6 - 6]   ▇           6     0        0     6   0      200   200
#> # ℹ 1 more variable: top_n <chr>
```
