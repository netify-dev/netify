# Event data slice from UCDP on Myanmar

Event data from UCDP for Myanmar

## Usage

``` r
data(myanmar)
```

## Format

A data frame with dyadic event observations.

## Source

[ucdp](https://ucdp.uu.se/)

## References

add UCDP reference ([ucdp](https://ucdp.uu.se/))

## Examples

``` r
data(myanmar)
myanmar[1:3, ]
#> # A tibble: 3 × 49
#>      id relid     year active_year code_status type_of_violence conflict_dset_id
#>   <dbl> <chr>    <dbl> <lgl>       <chr>                  <dbl> <chr>           
#> 1 48528 MYA-199…  1992 TRUE        Clear                      3 144             
#> 2 48616 MYA-199…  1992 TRUE        Clear                      3 144             
#> 3 48628 MYA-199…  1992 TRUE        Clear                      1 221             
#> # ℹ 42 more variables: conflict_new_id <dbl>, conflict_name <chr>,
#> #   dyad_dset_id <chr>, dyad_new_id <dbl>, dyad_name <chr>,
#> #   side_a_dset_id <chr>, side_a_new_id <dbl>, side_a <chr>,
#> #   side_b_dset_id <chr>, side_b_new_id <dbl>, side_b <chr>,
#> #   number_of_sources <dbl>, source_article <chr>, source_office <chr>,
#> #   source_date <chr>, source_headline <chr>, source_original <chr>,
#> #   where_prec <dbl>, where_coordinates <chr>, where_description <chr>, …
```
