# Add county, FIPS, and geometry to data frame with zip codes

Add county, FIPS, and geometry to data frame with zip codes

## Usage

``` r
add_counties(df, state, zip, fips = FALSE, geo = FALSE, sf = FALSE)
```

## Arguments

- df:

  data frame

- state:

  bare column name column containing state abbreviations

- zip:

  bare column name containing zip codes

- fips:

  add county FIPS code column, default is `FALSE`

- geo:

  add county geometry column, default is `FALSE`

- sf:

  convert to an `sf` object, default is `FALSE`

## Examples

``` r
(ex <- dplyr::tibble(state = "GA", zip = "31605"))
#> # A tibble: 1 × 2
#>   state zip  
#>   <chr> <chr>
#> 1 GA    31605

# Add county and latitude/longitude
ex |> add_counties(state, zip)
#> # A tibble: 1 × 5
#>   state zip   county    lat   lng
#>   <chr> <chr> <chr>   <dbl> <dbl>
#> 1 GA    31605 Lowndes  31.0 -83.2

# Add county FIPS
ex |> add_counties(state, zip, fips = TRUE)
#> # A tibble: 1 × 6
#>   state zip   county    lat   lng county_fips
#>   <chr> <chr> <chr>   <dbl> <dbl> <chr>      
#> 1 GA    31605 Lowndes  31.0 -83.2 13185      

# Add county `geometry` column,
# based on county FIPS column
ex |> add_counties(state, zip, fips = TRUE, geo = TRUE)
#> # A tibble: 1 × 7
#>   state zip   county    lat   lng county_fips geometry         
#>   <chr> <chr> <chr>   <dbl> <dbl> <chr>       <list>           
#> 1 GA    31605 Lowndes  31.0 -83.2 13185       <MULTIPOLYGON...>

# Convert to an `sf` object
ex |> add_counties(state, zip, fips = TRUE, geo = TRUE, sf = TRUE)
#> Simple feature collection with 1 feature and 4 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -83.23 ymin: 30.95 xmax: -83.23 ymax: 30.95
#> Geodetic CRS:  WGS 84
#> # A tibble: 1 × 5
#>   state zip   county  county_fips       geometry
#> * <chr> <chr> <chr>   <chr>          <POINT [°]>
#> 1 GA    31605 Lowndes 13185       (-83.23 30.95)
```
