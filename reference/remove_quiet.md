# Remove empty rows and columns

Remove empty rows and columns

## Usage

``` r
remove_quiet(df, ...)
```

## Arguments

- df:

  data frame

- ...:

  additional arguments to pass to
  [`janitor::remove_empty()`](https://sfirke.github.io/janitor/reference/remove_empty.html)

## Examples

``` r
dplyr::tibble(
  x = c(1, 2, NA),
  y = c(NA, NA, NA)) |>
  remove_quiet()
#> # A tibble: 2 × 1
#>       x
#>   <dbl>
#> 1     1
#> 2     2
```
