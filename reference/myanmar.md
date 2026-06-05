# event data slice from ucdp on myanmar

event data from ucdp for myanmar

## Usage

``` r
data(myanmar)
```

## Format

a data frame with dyadic event observations.

## Source

ucdp georeferenced event dataset, version 23.1.
<https://ucdp.uu.se/downloads/>

## References

davies, s., pettersson, t., and oberg, m. (2023). organized violence
1989-2022, and the return of conflict between states. journal of peace
research, 60(4), 691-708.
[doi:10.1177/00223433231185169](https://doi.org/10.1177/00223433231185169)
.

sundberg, r. and melander, e. (2013). introducing the ucdp georeferenced
event dataset. journal of peace research, 50(4), 523-532.
[doi:10.1177/0022343313484347](https://doi.org/10.1177/0022343313484347)
.

## Author

cassy dorff, shahryar minhas

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
