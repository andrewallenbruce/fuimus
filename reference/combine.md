# Combine multiple columns into one

Combine multiple columns into one

## Usage

``` r
combine(data, name = combined, columns, sep = "-")
```

## Arguments

- data:

  `<data.frame>` or `<tibble>`

- name:

  new column name, unquoted, default is `combined`

- columns:

  `<chr>` vector of columns to combine

- sep:

  separator between combined row data, default is `"-"`

## Value

A `<tibble>` with combined columns

## Examples

``` r
x <- mock_forager()

x[1, 1] <- ""

combine(x, payer, c("payer", "id"))
#> # A tibble: 10 × 8
#>    payer        class balance date_of_service date_of_release date_of_submission
#>    <chr>        <ord>   <dbl> <date>          <date>          <date>            
#>  1 UHC-         Prim…    51.5 2025-05-11      2025-05-30      2025-05-30        
#>  2 Medicare-00… Prim…    16.0 2025-07-11      2025-07-23      2025-07-27        
#>  3 Anthem-00003 Seco…   148.  2025-03-11      2025-03-27      2025-03-29        
#>  4 Medicare-00… Seco…    71.2 2025-08-11      2025-08-23      2025-08-24        
#>  5 Medicare-00… Seco…    99.7 2025-01-11      2025-01-22      2025-01-22        
#>  6 Medicaid-00… Prim…   350.  2025-01-11      2025-01-22      2025-01-25        
#>  7 UHC-00007    Prim…   166.  2025-06-11      2025-06-24      2025-06-24        
#>  8 BCBS-00008   Prim…   167.  2025-04-11      2025-04-20      2025-04-22        
#>  9 Medicare-00… Prim…    30.8 2024-12-11      2024-12-19      2024-12-23        
#> 10 Humana-00010 Prim…   122.  2025-05-11      2025-05-22      2025-05-24        
#> # ℹ 2 more variables: date_of_acceptance <date>, date_of_adjudication <date>
```
