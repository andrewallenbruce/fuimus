# String interpolation

Expressions enclosed by specified delimiters will be evaluated as R code
within the context of the `src` data/environment. The results will then
be inserted into the original string via
[`sprintf()`](https://rdrr.io/r/base/sprintf.html) i.e. string
interpolation.

## Usage

``` r
gluestick(fmt, src = parent.frame(), open = "{", close = "}", eval = TRUE)
```

## Arguments

- fmt:

  single `<character>` string containing the format specification.

- src:

  data source. An `environment`, `list`, `data.frame` or anything
  supported by
  [`as.environment()`](https://rdrr.io/r/base/as.environment.html).
  Default: [`parent.frame()`](https://rdrr.io/r/base/sys.parent.html)
  i.e. the calling environment

- open, close:

  the opening and closing `<character>` strings which delimit an
  expression. Default: [`{}`](https://rdrr.io/r/base/Paren.html). Note:
  the delimiters can be more complex than just a single character

- eval:

  `<logical>`. Should the expressions be treated as R code to be
  evaluated? Default: TRUE means to treat the expressions as R code and
  evaluate. If FALSE, then no code evaluation will ever be done and
  expressions will be treated as only variable names in the given `src`
  data. This may be safer in some contexts e.g. for user supplied fmt
  strings.

## Value

A `<character>` string with the expressions replaced by their values

## Examples

``` r
gluestick("Hello {name}", list(name = '#RStats'))
#> [1] "Hello #RStats"

gluestick("Hello ~!name!~", list(name = '#RStats'), open = "~!", close = "!~")
#> [1] "Hello #RStats"

name <- '#RStats'
gluestick("Hello {name}")
#> [1] "Hello #RStats"
```
