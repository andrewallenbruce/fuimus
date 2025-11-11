# Print a named list

Print a named list

## Usage

``` r
print_ls(ls, prefix = "")
```

## Arguments

- ls:

  `<list>` to print

- prefix:

  `<chr>` to prepend to each line

## Value

`<list>` invisibly

## Examples

``` r
print_ls(list(a = 1, b = 2, c = 3))
#> a : 1
#> b : 2
#> c : 3
```
