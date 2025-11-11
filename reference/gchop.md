# Chop Vector by Group

Chop Vector by Group

## Usage

``` r
gchop(v, g)
```

## Arguments

- v:

  `<character>` vector

- g:

  `<integer>` group

## Value

`<list>`

## See also

Other tidyverse:
[`columns_to_character()`](https://andrewallenbruce.github.io/fuimus/reference/columns_to_character.md),
[`glue_chr()`](https://andrewallenbruce.github.io/fuimus/reference/glue_chr.md),
[`glue_data_chr()`](https://andrewallenbruce.github.io/fuimus/reference/glue_data_chr.md),
[`named_group_split()`](https://andrewallenbruce.github.io/fuimus/reference/named_group_split.md),
[`search_for()`](https://andrewallenbruce.github.io/fuimus/reference/search_for.md)

## Examples

``` r
(v <- c("222", "280", "3020", "8690", "G0294", "G8126"))
#> [1] "222"   "280"   "3020"  "8690"  "G0294" "G8126"

(g <- sample(1:2, size = length(v), replace = TRUE))
#> [1] 2 1 1 1 1 2

gchop(v, g)
#> [[1]]
#> [1] "222"
#> 
#> [[2]]
#> [1] "280"   "3020"  "8690"  "G0294"
#> 
#> [[3]]
#> [1] "G8126"
#> 
```
