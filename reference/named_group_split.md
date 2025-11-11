# Split a `<tibble>` by Groups with Named List Output

This function takes a `<tibble>`, groups it by one or more variables,
and splits the grouped data into a list. The resulting list has names
derived from the unique combinations of the grouping variables.

## Usage

``` r
named_group_split(df, ...)
```

## Arguments

- df:

  `<tibble>` or `<data.frame>` to split

- ...:

  One or more unquoted variables by which to group and then split `df`.
  Variables can be separated by commas.

## Value

named `<list>` of tibbles, the names of which are derived from the
unique combinations of grouping variables, separated by "\_".

## See also

Other tidyverse:
[`columns_to_character()`](https://andrewallenbruce.github.io/fuimus/reference/columns_to_character.md),
[`gchop()`](https://andrewallenbruce.github.io/fuimus/reference/gchop.md),
[`glue_chr()`](https://andrewallenbruce.github.io/fuimus/reference/glue_chr.md),
[`glue_data_chr()`](https://andrewallenbruce.github.io/fuimus/reference/glue_data_chr.md),
[`search_for()`](https://andrewallenbruce.github.io/fuimus/reference/search_for.md)

## Examples

``` r
(x <- dplyr::tibble(
  zip = c("Data_Weekly.zip",
        "Data_Weekly.zip",
        "Data_April.zip",
        "Deactivated.zip"),
  file = c(
    "npidata.csv",
    "npidata2.csv",
    "endpoint.csv",
    "Deactivated.xlsx")))
#> # A tibble: 4 × 2
#>   zip             file            
#>   <chr>           <chr>           
#> 1 Data_Weekly.zip npidata.csv     
#> 2 Data_Weekly.zip npidata2.csv    
#> 3 Data_April.zip  endpoint.csv    
#> 4 Deactivated.zip Deactivated.xlsx

named_group_split(x, zip)
#> $Data_April.zip
#> # A tibble: 1 × 2
#>   zip            file        
#>   <chr>          <chr>       
#> 1 Data_April.zip endpoint.csv
#> 
#> $Data_Weekly.zip
#> # A tibble: 2 × 2
#>   zip             file        
#>   <chr>           <chr>       
#> 1 Data_Weekly.zip npidata.csv 
#> 2 Data_Weekly.zip npidata2.csv
#> 
#> $Deactivated.zip
#> # A tibble: 1 × 2
#>   zip             file            
#>   <chr>           <chr>           
#> 1 Deactivated.zip Deactivated.xlsx
#> 
```
