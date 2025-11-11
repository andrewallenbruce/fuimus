# Generate a sequence of numbers with a new prefix

Generate a sequence of numbers with a new prefix

## Usage

``` r
rename_seq(
  n,
  new,
  between = " = ",
  old,
  collapse = ", ",
  enclose = NULL,
  style = TRUE
)
```

## Arguments

- n:

  `<int>` Numeric sequence to generate

- new:

  `<chr>` New prefix

- between:

  `<chr>` Separator between `new` and `old`, default `" = "`

- old:

  `<chr>` Old prefix

- collapse:

  `<chr>` Separator between sequences, default `", "`

- enclose:

  `<chr>` *(optional)* Vector of `length(x) == 2` with which to enclose
  output

- style:

  `<lgl>` Apply
  [`styler::style_text()`](https://styler.r-lib.org/reference/style_text.html)
  to output, default `TRUE`

## Value

`<chr>` collapsed vector of `n` sequences

## Examples

``` r
rename_seq(
   n        = 10,
   new      = "id_issuer_",
   between  = " = ",
   old      = "Other.ID.Issuer.",
   enclose  = c("x = c(", ")"),
   collapse = ",\n ",
   style    = TRUE)
#> x <- c(
#>   id_issuer_1 = Other.ID.Issuer.1,
#>   id_issuer_2 = Other.ID.Issuer.2,
#>   id_issuer_3 = Other.ID.Issuer.3,
#>   id_issuer_4 = Other.ID.Issuer.4,
#>   id_issuer_5 = Other.ID.Issuer.5,
#>   id_issuer_6 = Other.ID.Issuer.6,
#>   id_issuer_7 = Other.ID.Issuer.7,
#>   id_issuer_8 = Other.ID.Issuer.8,
#>   id_issuer_9 = Other.ID.Issuer.9,
#>   id_issuer_10 = Other.ID.Issuer.10
#> )
```
