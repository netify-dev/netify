# Convert netify objects back to dyadic data frames

`unnetify` (also available as `netify_to_df`) reverses the netify
transformation by converting network objects back into dyadic
(edge-level) data frames. this function combines network structure with
any associated nodal and dyadic attributes, creating a data frame where
each row represents a dyad (edge) with all relevant attributes attached.

## Usage

``` r
unnetify(netlet, remove_zeros = FALSE)

netify_to_df(netlet, remove_zeros = FALSE)
```

## Arguments

- netlet:

  a netify object to be converted to dyadic format.

- remove_zeros:

  logical. if TRUE, removes dyads with zero edge weights from the
  output, resulting in a data frame of only non-zero relationships. if
  FALSE (default), includes all possible dyads in the network.

## Value

a data frame with one row per dyad containing:

- **from**: name/id of the first actor in the dyad

- **to**: name/id of the second actor in the dyad

- **time**: time period (for longitudinal networks)

- **weight column**: edge weight values (column named after the weight
  parameter used in netify)

- **from_id**: unique identifier combining actor and time (longitudinal
  only)

- **to_id**: unique identifier combining actor and time (longitudinal
  only)

- **dyadic attributes**: any edge-level covariates from the original
  data

- **nodal attributes**: actor-level covariates merged onto dyads,
  suffixed with "\_from" and "\_to" to match the corresponding actor

## Details

this function essentially reverses the netify process, making it useful
for:

- exporting network data for analysis in other software

- creating dyadic datasets for regression analysis

- inspecting network data in familiar data frame format

- merging network results back with other dyadic covariates

nodal attributes are attached twice per dyad - once for the "from" actor
(suffixed "\_from") and once for the "to" actor (suffixed "\_to"). this
applies to both directed and undirected networks, ensuring that both
actors' attributes are available for dyadic analysis.

the function handles both cross-sectional and longitudinal netify
objects, automatically detecting the structure and adjusting the output
accordingly.

## Note

for large networks, setting `remove_zeros = FALSE` can result in very
large data frames, as it includes all possible dyads (n x (n-1) for
directed networks or n x (n-1) / 2 for undirected networks).

## Author

cassy dorff, shahryar minhas

## Examples

``` r
# load example data
data(icews)

# create a netify object with attributes
icews_10 <- icews[icews$year == 2010, ]

verbCoop_net <- netify(
    icews_10,
    actor1 = "i", actor2 = "j",
    symmetric = FALSE,
    weight = "verbCoop",
    nodal_vars = c("i_polity2", "i_log_gdp"),
    dyad_vars = c("verbConf", "matlConf")
)

# convert back to dyadic data frame
dyad_df <- unnetify(verbCoop_net)

# examine structure
head(dyad_df)
#>          from        to time verbCoop verbConf matlConf       from_id
#> 1 Afghanistan   Albania    1        0        0        2 Afghanistan_1
#> 2 Afghanistan   Algeria    1        0        0        1 Afghanistan_1
#> 3 Afghanistan    Angola    1        0        0        0 Afghanistan_1
#> 4 Afghanistan Argentina    1        1        0        0 Afghanistan_1
#> 5 Afghanistan   Armenia    1        7        0        0 Afghanistan_1
#> 6 Afghanistan Australia    1      125        2       24 Afghanistan_1
#>         to_id i_polity2_from i_polity2_to i_log_gdp_from i_log_gdp_to
#> 1   Albania_1             NA            9       23.49884     23.06701
#> 2   Algeria_1             NA            2       23.49884     25.67186
#> 3    Angola_1             NA           -2       23.49884     24.97089
#> 4 Argentina_1             NA            8       23.49884     27.03815
#> 5   Armenia_1             NA            5       23.49884     22.86492
#> 6 Australia_1             NA           10       23.49884     27.79745
names(dyad_df)
#>  [1] "from"           "to"             "time"           "verbCoop"      
#>  [5] "verbConf"       "matlConf"       "from_id"        "to_id"         
#>  [9] "i_polity2_from" "i_polity2_to"   "i_log_gdp_from" "i_log_gdp_to"  

# remove zero-weight dyads for more compact output
dyad_df_nonzero <- unnetify(verbCoop_net, remove_zeros = TRUE)
nrow(dyad_df_nonzero) # much smaller than full dyadic dataset
#> [1] 9976

# note how nodal attributes are added
# for directed network: _from and _to suffixes
head(dyad_df[, c("from", "to", "i_polity2_from", "i_polity2_to")])
#>          from        to i_polity2_from i_polity2_to
#> 1 Afghanistan   Albania             NA            9
#> 2 Afghanistan   Algeria             NA            2
#> 3 Afghanistan    Angola             NA           -2
#> 4 Afghanistan Argentina             NA            8
#> 5 Afghanistan   Armenia             NA            5
#> 6 Afghanistan Australia             NA           10

# longitudinal example
verbCoop_longit <- netify(
    icews,
    actor1 = "i", actor2 = "j",
    time = "year",
    symmetric = FALSE,
    weight = "verbCoop",
    nodal_vars = c("i_polity2", "i_log_gdp")
)

# convert longitudinal network
dyad_df_longit <- unnetify(verbCoop_longit, remove_zeros = TRUE)

# check time periods are included
table(dyad_df_longit$time)
#> 
#> 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 
#> 8692 8887 9514 9346 9429 9739 9988 9854 9976 9708 9841 9911 9774 

# each dyad now has associated time period
# note: weight column is named after the weight variable (verbCoop)
head(dyad_df_longit[, c("from", "to", "time", "verbCoop")])
#>          from         to time verbCoop
#> 1 Afghanistan    Albania 2002        6
#> 2 Afghanistan    Armenia 2002        3
#> 3 Afghanistan  Australia 2002       81
#> 4 Afghanistan    Austria 2002       17
#> 5 Afghanistan Azerbaijan 2002       14
#> 6 Afghanistan    Bahrain 2002       10

# use the output for further analysis
# \donttest{
# for example, regression analysis
lm(verbCoop ~ i_polity2_from + i_polity2_to + verbConf, data = dyad_df)
#> 
#> Call:
#> lm(formula = verbCoop ~ i_polity2_from + i_polity2_to + verbConf, 
#>     data = dyad_df)
#> 
#> Coefficients:
#>    (Intercept)  i_polity2_from    i_polity2_to        verbConf  
#>        13.6092         -0.1287         -0.2820          4.2253  
#> 
# }
```
