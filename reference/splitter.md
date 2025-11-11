# Wrapper for [`strsplit()`](https://rdrr.io/r/base/strsplit.html) that unlists and unnames results

Wrapper for [`strsplit()`](https://rdrr.io/r/base/strsplit.html) that
unlists and unnames results

## Usage

``` r
splitter(x)
```

## Arguments

- x:

  `<chr>` string or named `<list>` of `<chr>` strings

## Value

An unnamed `<list>` of split `<chr>` vectors

## Examples

``` r
# unnamed vector
splitter("XYZ")
#> [1] "X" "Y" "Z"

# named vector
splitter(c(x = "XYZ"))
#> [1] "X" "Y" "Z"

# unnamed list with one element
splitter(list("XYZ"))
#> [1] "X" "Y" "Z"

# unnamed list with multiple elements
splitter(list("YYY", "ZZZ"))
#> [[1]]
#> [1] "Y" "Y" "Y"
#> 
#> [[2]]
#> [1] "Z" "Z" "Z"
#> 

# named list with one element
splitter(list(x = "XYZ"))
#> [1] "X" "Y" "Z"

# named list with multiple elements
splitter(list(x = "YYY", xx = "ZZZ"))
#> [[1]]
#> [1] "Y" "Y" "Y"
#> 
#> [[2]]
#> [1] "Z" "Z" "Z"
#> 
```
