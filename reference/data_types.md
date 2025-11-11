# Return data.frame of column types

Return data.frame of column types

## Usage

``` r
data_types(data)
```

## Arguments

- data:

  `<data.frame>`

## Value

`<data.frame>` or `<tibble>`

## Examples

``` r
(x <- dplyr::tibble(int = c(1:9, NA_integer_), chr = letters[1:10], date = Sys.Date()))
#> # A tibble: 10 × 3
#>      int chr   date      
#>    <int> <chr> <date>    
#>  1     1 a     2025-11-11
#>  2     2 b     2025-11-11
#>  3     3 c     2025-11-11
#>  4     4 d     2025-11-11
#>  5     5 e     2025-11-11
#>  6     6 f     2025-11-11
#>  7     7 g     2025-11-11
#>  8     8 h     2025-11-11
#>  9     9 i     2025-11-11
#> 10    NA j     2025-11-11

data_types(x)
#> # A tibble: 3 × 3
#>   column type      missing
#>   <chr>  <chr>       <int>
#> 1 int    integer         1
#> 2 chr    character       0
#> 3 date   date            0
```
