# Generate code from a vector of values

Generate code from a vector of values

## Usage

``` r
create_vec(x, collapse = ", ", enclose = c("x = c(", ")"), style = TRUE, ...)
```

## Arguments

- x:

  `<vec>` vector

- collapse:

  `<chr>` Separator between sequences, default `", "`

- enclose:

  `<chr>` *(optional)* Vector of `length(x) == 2` with which to enclose
  output

- style:

  `<lgl>` Apply
  [`styler::style_text()`](https://styler.r-lib.org/reference/style_text.html)
  to output, default `TRUE`

- ...:

  Additional arguments passed to
  [`styler::tidyverse_style()`](https://styler.r-lib.org/reference/tidyverse_style.html)

## Value

`<chr>` vector of valid R code

## Examples

``` r
hcpcs <- c(
   "63091", "81536", "99305", "63012",
   "78835", "E0185", "44408", "87485",
   "36015", "0581F", "33478", "21184",
   "42999", "15155", "76705", "23412",
   "99406", "0585T", "0272T", "92507")

create_vec(x = hcpcs)
#> x <- c("63091", "81536", "99305", "63012", "78835", "E0185", "44408", "87485", "36015", "0581F", "33478", "21184", "42999", "15155", "76705", "23412", "99406", "0585T", "0272T", "92507")
```
