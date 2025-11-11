# Search a data frame column by string

Search a data frame column by string

## Usage

``` r
search_for(data, column, search, ignore = TRUE, ...)
```

## Arguments

- data:

  `<data.frame>` or `<tibble>`

- column:

  `<chr>` string, column to search

- search:

  `<chr>` string to search for, can be a regex

- ignore:

  `<lgl>` ignore case, default is `TRUE`

- ...:

  additional arguments

## Value

`<data.frame>` or `<tibble>`

## See also

Other tidyverse:
[`columns_to_character()`](https://andrewallenbruce.github.io/fuimus/reference/columns_to_character.md),
[`gchop()`](https://andrewallenbruce.github.io/fuimus/reference/gchop.md),
[`glue_chr()`](https://andrewallenbruce.github.io/fuimus/reference/glue_chr.md),
[`glue_data_chr()`](https://andrewallenbruce.github.io/fuimus/reference/glue_data_chr.md),
[`named_group_split()`](https://andrewallenbruce.github.io/fuimus/reference/named_group_split.md)

## Examples

``` r
(x <- dplyr::tibble(y = 1:10, z = letters[1:10]))
#> # A tibble: 10 × 2
#>        y z    
#>    <int> <chr>
#>  1     1 a    
#>  2     2 b    
#>  3     3 c    
#>  4     4 d    
#>  5     5 e    
#>  6     6 f    
#>  7     7 g    
#>  8     8 h    
#>  9     9 i    
#> 10    10 j    

search_for(data = x, column = "z", search = "[a|j]")
#> # A tibble: 2 × 2
#>       y z    
#>   <int> <chr>
#> 1     1 a    
#> 2    10 j    
```
