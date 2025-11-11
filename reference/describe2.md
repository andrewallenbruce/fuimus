# Describe 2

Describe 2

## Usage

``` r
describe2(df, ...)
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
describe2(mock_provider(2000:2020))
#> Warning: 'cheapr::enframe_' is deprecated.
#> Use 'fastplyr::f_enframe' instead.
#> See help("Deprecated")
#> # A tibble: 3 × 13
#>   column type       n   min   mean    med   max   iqr     sd    mad distribution
#>   <chr>  <chr>  <int> <int>  <dbl>  <dbl> <int> <dbl>  <dbl>  <dbl> <chr>       
#> 1 year   <int>  84420  2000  2010   2010   2020   10  6.13e0 7.41e0 ▇▅▅▅▅▅▅▅▅▅  
#> 2 payme… <int> 631392 10237 15033. 14560. 19970 6159. 3.29e3 4.51e3 ▅▅▃▂▅▁▁▅▇▃  
#> 3 group  <chr>     42     1     1      1      1    0  0      0      ▇           
#> # ℹ 2 more variables: uniq <int>, top <chr>

describe2(mock_forager(200))
#> Warning: 'cheapr::enframe_' is deprecated.
#> Use 'fastplyr::f_enframe' instead.
#> See help("Deprecated")
#> # A tibble: 4 × 13
#>   column  type       n   min   mean   med   max   iqr     sd   mad distribution
#>   <chr>   <chr>  <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl>  <dbl> <dbl> <chr>       
#> 1 class   <ord>   294   1      1.47    1     2     1   0.500  0    ▇▁▁▁▁▁▁▁▁▇  
#> 2 payer   <fct>  1045   1      5.22    6     9     4   2.55   2.97 ▇▃▃▃▅▅▇▃    
#> 3 balance <dbl> 27924.  8.47 140.    124.  513.  111. 92.8   80.9  ▅▇▇▅▂▂▁▁▁▁▁ 
#> 4 id      <chr>  1200   6      6       6     6     0   0      0    ▇           
#> # ℹ 2 more variables: uniq <int>, top <chr>
```
