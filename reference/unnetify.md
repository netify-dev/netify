# Convert netify objects back to dyadic data frames

`unnetify` (also available as `netify_to_df`) reverses the netify
transformation by converting network objects back into dyadic
(edge-level) data frames. This function combines network structure with
any associated nodal and dyadic attributes, creating a data frame where
each row represents a dyad (edge) with all relevant attributes attached.

## Usage

``` r
unnetify(netlet, remove_zeros = FALSE)

netify_to_df(netlet, remove_zeros = FALSE)
```

## Arguments

- netlet:

  A netify object to be converted to dyadic format.

- remove_zeros:

  Logical. If TRUE, removes dyads with zero edge weights from the
  output, resulting in a data frame of only non-zero relationships. If
  FALSE (default), includes all possible dyads in the network.

## Value

A data frame with one row per dyad containing:

- **from**: Name/ID of the first actor in the dyad

- **to**: Name/ID of the second actor in the dyad

- **time**: Time period (for longitudinal networks)

- **weight column**: Edge weight values (column named after the weight
  parameter used in netify)

- **from_id**: Unique identifier combining actor and time (longitudinal
  only)

- **to_id**: Unique identifier combining actor and time (longitudinal
  only)

- **Dyadic attributes**: Any edge-level covariates from the original
  data

- **Nodal attributes**: Actor-level covariates merged onto dyads:

  - For directed networks: suffixed with "\_from" and "\_to"

  - For undirected networks: suffixed with "\_dyad"

## Details

This function essentially reverses the netify process, making it useful
for:

- Exporting network data for analysis in other software

- Creating dyadic datasets for regression analysis

- Inspecting network data in familiar data frame format

- Merging network results back with other dyadic covariates

For directed networks, nodal attributes are attached twice - once for
the sender (suffixed "\_from") and once for the receiver (suffixed
"\_to"). This allows for modeling sender and receiver effects
separately.

For undirected networks, nodal attributes are attached once per dyad
with the suffix "\_dyad", since there is no meaningful distinction
between sender and receiver.

The function handles both cross-sectional and longitudinal netify
objects, automatically detecting the structure and adjusting the output
accordingly.

## Note

For large networks, setting `remove_zeros = FALSE` can result in very
large data frames, as it includes all possible dyads (n × (n-1) for
directed networks or n × (n-1) / 2 for undirected networks).

## Author

Cassy Dorff, Shahryar Minhas

## Examples

``` r
# Load example data
data(icews)

# Create a netify object with attributes
icews_10 <- icews[icews$year == 2010, ]

verbCoop_net <- netify(
    icews_10,
    actor1 = "i", actor2 = "j",
    symmetric = FALSE,
    weight = "verbCoop",
    nodal_vars = c("i_polity2", "i_log_gdp"),
    dyad_vars = c("verbConf", "matlConf")
)

# Convert back to dyadic data frame
dyad_df <- unnetify(verbCoop_net)

# Examine structure
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

# Remove zero-weight dyads for more compact output
dyad_df_nonzero <- unnetify(verbCoop_net, remove_zeros = TRUE)
nrow(dyad_df_nonzero) # Much smaller than full dyadic dataset
#> [1] 9976

# Note how nodal attributes are added
# For directed network: _from and _to suffixes
head(dyad_df[, c("from", "to", "i_polity2_from", "i_polity2_to")])
#>          from        to i_polity2_from i_polity2_to
#> 1 Afghanistan   Albania             NA            9
#> 2 Afghanistan   Algeria             NA            2
#> 3 Afghanistan    Angola             NA           -2
#> 4 Afghanistan Argentina             NA            8
#> 5 Afghanistan   Armenia             NA            5
#> 6 Afghanistan Australia             NA           10

# Longitudinal example
verbCoop_longit <- netify(
    icews,
    actor1 = "i", actor2 = "j",
    time = "year",
    symmetric = FALSE,
    weight = "verbCoop",
    nodal_vars = c("i_polity2", "i_log_gdp")
)

# Convert longitudinal network
dyad_df_longit <- unnetify(verbCoop_longit, remove_zeros = TRUE)

# Check time periods are included
table(dyad_df_longit$time)
#> 
#> 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 
#> 8692 8887 9514 9346 9429 9739 9988 9854 9976 9708 9841 9911 9774 

# Each dyad now has associated time period
# Note: weight column is named after the weight variable (verbCoop)
head(dyad_df_longit[, c("from", "to", "time", "verbCoop")])
#>          from         to time verbCoop
#> 1 Afghanistan    Albania 2002        6
#> 2 Afghanistan    Armenia 2002        3
#> 3 Afghanistan  Australia 2002       81
#> 4 Afghanistan    Austria 2002       17
#> 5 Afghanistan Azerbaijan 2002       14
#> 6 Afghanistan    Bahrain 2002       10

# Use the output for further analysis
if (FALSE) { # \dontrun{
# For example, regression analysis
lm(verbCoop ~ i_polity2_from + i_polity2_to + verbConf, data = dyad_df)
} # }
```
