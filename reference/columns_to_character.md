# Coerce all columns to type character

Coerce all columns to type character

## Usage

``` r
columns_to_character(data)
```

## Arguments

- data:

  `<data.frame>`

## Value

`<data.frame>` with columns coerced to character

## See also

Other tidyverse:
[`gchop()`](https://andrewallenbruce.github.io/fuimus/reference/gchop.md),
[`glue_chr()`](https://andrewallenbruce.github.io/fuimus/reference/glue_chr.md),
[`glue_data_chr()`](https://andrewallenbruce.github.io/fuimus/reference/glue_data_chr.md),
[`named_group_split()`](https://andrewallenbruce.github.io/fuimus/reference/named_group_split.md),
[`search_for()`](https://andrewallenbruce.github.io/fuimus/reference/search_for.md)

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

columns_to_character(x)
#> # A tibble: 10 × 3
#>    int   chr   date      
#>    <chr> <chr> <chr>     
#>  1 1     a     2025-11-11
#>  2 2     b     2025-11-11
#>  3 3     c     2025-11-11
#>  4 4     d     2025-11-11
#>  5 5     e     2025-11-11
#>  6 6     f     2025-11-11
#>  7 7     g     2025-11-11
#>  8 8     h     2025-11-11
#>  9 9     i     2025-11-11
#> 10 NA    j     2025-11-11
```
