# Describe unique values in a data frame

Describe unique values in a data frame

## Usage

``` r
describe_unique(
  df,
  ...,
  .rename_first = "value",
  .set_names = NULL,
  .names_to = "variable"
)
```

## Arguments

- df:

  data frame

- ...:

  unquoted data frame columns

- .rename_first:

  default is `"value"`

- .set_names:

  vector of names

- .names_to:

  default is `"variable"`

## Value

a data frame with unique values

## Examples

``` r
describe_unique(mock_forager(), class, payer)
#> # A tibble: 7 × 3
#>   variable value         n
#>   <chr>    <chr>     <int>
#> 1 class    Primary       5
#> 2 class    Secondary     5
#> 3 payer    Anthem        3
#> 4 payer    Cigna         3
#> 5 payer    Centene       2
#> 6 payer    BCBS          1
#> 7 payer    Medicaid      1

describe_unique(
  mock_forager(50),
  names(mock_forager())[c(2:3, 6:9)]) |>
  dplyr::filter(n > 2) |>
  print(n = 30)
#> # A tibble: 13 × 3
#>    variable           value          n
#>    <chr>              <chr>      <int>
#>  1 payer              BCBS           9
#>  2 payer              Anthem         8
#>  3 payer              Medicaid       8
#>  4 payer              Cigna          6
#>  5 payer              Medicare       6
#>  6 payer              Humana         5
#>  7 payer              Centene        4
#>  8 payer              UHC            4
#>  9 class              Secondary     26
#> 10 class              Primary       24
#> 11 date_of_release    2025-01-26     3
#> 12 date_of_acceptance 2025-08-30     3
#> 13 date_of_acceptance 2025-10-27     3
```
