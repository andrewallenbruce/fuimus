# Calculate Geometric Mean

Average
[`rate_of_return()`](https://andrewallenbruce.github.io/fuimus/reference/rate_of_return.md)

## Usage

``` r
geomean(x)
```

## Arguments

- x:

  numeric vector

## Examples

``` r
x <- mock_provider(2020:2025)

x |>
  dplyr::group_by(group) |>
  rate_of_return(payment)
#> # A tibble: 12 × 4
#>     year group payment payment_ror
#>    <int> <chr>   <int>       <dbl>
#>  1  2020 A       17944       1    
#>  2  2021 A       12275       0.684
#>  3  2022 A       18158       1.48 
#>  4  2023 A       11637       0.641
#>  5  2024 A       14338       1.23 
#>  6  2025 A       13811       0.963
#>  7  2020 B       17088       1    
#>  8  2021 B       10385       0.608
#>  9  2022 B       11437       1.10 
#> 10  2023 B       17002       1.49 
#> 11  2024 B       11229       0.660
#> 12  2025 B       19231       1.71 

x |>
  dplyr::group_by(group) |>
  rate_of_return(payment) |>
  dplyr::summarise(gmean = geomean(payment_ror), .by = group)
#> # A tibble: 2 × 2
#>   group gmean
#>   <chr> <dbl>
#> 1 A     0.957
#> 2 B     1.02 
```
