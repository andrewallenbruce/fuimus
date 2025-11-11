# Replace All Fixed Strings

Replace All Fixed Strings

## Usage

``` r
replace_fixed(x, p, r, v = FALSE)
```

## Arguments

- x:

  `<chr>` vector or string to search in

- p:

  `<chr>` vector of search patterns

- r:

  `<chr>` vector of replacements for matched patterns

- v:

  `<lgl>` default is FALSE; should each occurrence of a pattern in every
  string be replaced by a corresponding replacement string?

## Value

`<chr>` vector with all occurrences of each pattern replaced

## Examples

``` r
if (FALSE) {
replace_fixed(
  x,
  c(":", "%", "@", "$"),
  c("_", "", "", ""))
}
```
