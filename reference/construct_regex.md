# Construct regex patterns

Construct regex patterns

## Usage

``` r
construct_regex(x)
```

## Arguments

- x:

  `<chr>` vector

## Value

`<chr>` regex vector

## Examples

``` r
# construct_regex(search_descriptions()$hcpcs_code)

# construct_regex(search_adjustments()$adj_code)

# Incorrect output:
"^[A-DM-PWY1-9][AIOR0-9]?[0-9]?{3}$"
#> [1] "^[A-DM-PWY1-9][AIOR0-9]?[0-9]?{3}$"

# Digits at the end should be:
"^[A-DM-PWY1-9][AIOR0-9]?[0-9]{0,3}?$"
#> [1] "^[A-DM-PWY1-9][AIOR0-9]?[0-9]{0,3}?$"

# Test adj codes
adj <- c("4", "CO", "P6", "100",
         "B19", "MA22", "MA124", "N766")

construct_regex(adj)
#> [1] "^[BCMNP14][AO0167]?[0-269]?[26]?[4]?$"
```
