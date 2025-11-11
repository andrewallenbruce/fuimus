# Color text using HTML color styling

Useful in conjuction with ggtext and glue

## Usage

``` r
color_text(text, hex_code)
```

## Arguments

- text:

  `<character>` string of text to

- hex_code:

  `<character>` string, color hex code

## Value

`<character>` string

## Examples

``` r
color_text("Hello there!", "#BD43BF")
#> [1] "<span style='color:#BD43BF'>Hello there!</span>"
```
