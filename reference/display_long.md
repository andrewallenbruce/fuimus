# Pivot data frame to long format for easy printing

Pivot data frame to long format for easy printing

## Usage

``` r
display_long(data)
```

## Arguments

- data:

  `<data.frame>`

## Value

a `<data.frame>` or `<tibble>` in long format

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

display_long(x)
#> # A tibble: 30 × 2
#>    name  value     
#>    <chr> <chr>     
#>  1 int   1         
#>  2 chr   a         
#>  3 date  2025-11-11
#>  4 int   2         
#>  5 chr   b         
#>  6 date  2025-11-11
#>  7 int   3         
#>  8 chr   c         
#>  9 date  2025-11-11
#> 10 int   4         
#> # ℹ 20 more rows
```
