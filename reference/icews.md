# Event data slice from ICEWS

Event data from ICEWS for select countries from 2002 to 2014, and
additional nodal variables from the World Bank and Polity IV.

## Usage

``` r
data(icews)
```

## Source

[icews](https://dataverse.harvard.edu/dataverse/icews)

## References

add icews reference
([icews](https://dataverse.harvard.edu/dataverse/icews))

## Examples

``` r
data(icews)
icews[1:3, ]
#>             i       j year                       id verbCoop matlCoop verbConf
#> 2 Afghanistan Albania 2002 AFGHANISTAN_ALBANIA_2002        6        1        0
#> 3 Afghanistan Albania 2003 AFGHANISTAN_ALBANIA_2003        1        1        0
#> 4 Afghanistan Albania 2004 AFGHANISTAN_ALBANIA_2004       10        2        0
#>   matlConf           i_year       j_year i_polity2 j_polity2 i_iso3c j_iso3c
#> 2        0 AFGHANISTAN_2002 ALBANIA_2002        NA         7     AFG     ALB
#> 3        0 AFGHANISTAN_2003 ALBANIA_2003        NA         7     AFG     ALB
#> 4        1 AFGHANISTAN_2004 ALBANIA_2004        NA         7     AFG     ALB
#>     i_region              j_region      i_gdp      j_gdp i_log_gdp j_log_gdp
#> 2 South Asia Europe & Central Asia 7555185296 6857137321  22.74550  22.64856
#> 3 South Asia Europe & Central Asia 8222480251 7236243584  22.83014  22.70237
#> 4 South Asia Europe & Central Asia 8338755823 7635298387  22.84418  22.75605
#>      i_pop   j_pop i_log_pop j_log_pop
#> 2 21000256 3051010  16.86005  14.93098
#> 3 22645130 3039616  16.93546  14.92724
#> 4 23553551 3026939  16.97479  14.92306
```
