# Determine a common ordering from a set of vectors

Determine a common ordering from a set of vectors

## Usage

``` r
find_common_order(..., .strict = TRUE)
```

## Arguments

- ...:

  Input vectors

- .strict:

  If `TRUE`, an error will be thrown if no common ordering exists. If
  `FALSE`, a warning will be thrown and the output will be returned.
  Note that to use the output in such cases, you may want to call
  [`unique()`](https://rdrr.io/r/base/unique.html) to remove any
  duplicates.

## Value

`<character>` vector of `n` NPIs

## Examples

``` r
if (FALSE) {

find_common_order(
  c(1, 3, 4, 7, 8),
  c(2, 3, 4, 8, 9),
  c(4, 5, 6, 7, 9)
)

find_common_order(
  c("bananas", "oranges", "apples"),
  c("oranges", "avocados", "apples"),
  c("apples", "mangos", "lemons")
)

find_common_order(
  c("foo", "bar"),
  c("bar", "baz"),
  c("baz", "foo")
)
}
```
