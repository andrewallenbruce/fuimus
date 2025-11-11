# Format multiple line character vector to single line

Format multiple line character vector to single line

## Usage

``` r
single_line_string(x)
```

## Arguments

- x:

  `<chr>` character vector with line breaks (`\n`)

## Value

`<chr>` single line character vector

## Examples

``` r
single_line_string(
   "(Jan(?:uary)?|
    Feb(?:ruary)?|
    Mar(?:ch)?|"
 )
#> [1] "(Jan(?:uary)?|Feb(?:ruary)?|Mar(?:ch)?|"
```
