# Event data slice from UCDP on Mexico

Event data from UCDP for Mexico

## Usage

``` r
data(mexico)
```

## Source

[ucdp](https://ucdp.uu.se/)

## References

add UCDP reference ([ucdp](https://ucdp.uu.se/))

## Examples

``` r
data(mexico)
mexico[1:3, ]
#> # A tibble: 3 × 49
#>       id relid    year active_year code_status type_of_violence conflict_dset_id
#>    <dbl> <chr>   <dbl> <lgl>       <chr>                  <dbl> <chr>           
#> 1 182027 MEX-19…  1992 FALSE       Clear                      2 14353           
#> 2 182028 MEX-19…  1992 FALSE       Clear                      2 14353           
#> 3 182047 MEX-19…  1989 FALSE       Clear                      3 10              
#> # ℹ 42 more variables: conflict_new_id <dbl>, conflict_name <chr>,
#> #   dyad_dset_id <chr>, dyad_new_id <dbl>, dyad_name <chr>,
#> #   side_a_dset_id <chr>, side_a_new_id <dbl>, side_a <chr>,
#> #   side_b_dset_id <chr>, side_b_new_id <dbl>, side_b <chr>,
#> #   number_of_sources <dbl>, source_article <chr>, source_office <chr>,
#> #   source_date <chr>, source_headline <chr>, source_original <chr>,
#> #   where_prec <dbl>, where_coordinates <chr>, where_description <chr>, …
```
