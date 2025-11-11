# Timestamp

Timestamp

## Usage

``` r
ttimestamp(time = Sys.time())
```

## Arguments

- time:

  Sys.time() or a date-time object

## Value

A list()

## Examples

``` r
timestamp()
#> ##------ Tue Nov 11 00:11:25 2025 ------##

date()
#> [1] "Tue Nov 11 00:11:25 2025"

ttimestamp()
#> [[1]]
#> [1] "2025-11-11 00:11:25"
#> 
#> [[2]]
#> [1] "Tue Nov 11 00:11:25 2025"
#> 
```
