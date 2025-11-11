# Common Regular expressions

Common Regular expressions

## Usage

``` r
common_regex(x = c("month_date", "month", "url", "date"))
```

## Arguments

- x:

  `<chr>` regex name

## Value

`<chr>` string of a regex

## Examples

``` r
common_regex("url")
#> [1] "^(?:(?:http(?:s)?|ftp)://)(?:\\S+(?::(?:\\S)*)?@)?(?:(?:[a-z0-9¡-\uffff](?:-)*)*(?:[a-z0-9¡-\uffff])+)(?:\\.(?:[a-z0-9¡-\uffff](?:-)*)*(?:[a-z0-9¡-\uffff])+)*(?:\\.(?:[a-z0-9¡-\uffff]){2,})(?::(?:\\d){2,5})?(?:/(?:\\S)*)?$"

common_regex("month")
#> [1] "(Jan(?:uary)?|Feb(?:ruary)?|Mar(?:ch)?|Apr(?:il)?|May|Jun(?:e)?|Jul(?:y)?|Aug(?:ust)?|Sep(?:tember)?|Oct(?:ober)?|Nov(?:ember)?|Dec(?:ember)?)"

common_regex("month_date")
#> [1] "(Jan(?:uary)?|Feb(?:ruary)?|Mar(?:ch)?|Apr(?:il)?|May|Jun(?:e)?|Jul(?:y)?|Aug(?:ust)?|Sep(?:tember)?|Oct(?:ober)?|Nov(?:ember)?|Dec(?:ember)?)\\s+(\\d{1,2})\\,\\s+(\\d{4})"
```
