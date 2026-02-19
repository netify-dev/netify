# Decompose a netify object into edge and node data frames

`decompose_netify` separates a netify object into its constituent parts:
a data frame of edges with attributes and a data frame of nodal
attributes.

## Usage

``` r
decompose_netify(netlet, remove_zeros = TRUE)
```

## Arguments

- netlet:

  A netify object (class "netify") to be decomposed.

- remove_zeros:

  Logical. If TRUE (default), edges with zero weight values are removed
  from the edge data frame. If FALSE, zero-weight edges are retained.

## Value

A list containing two data frames:

- **edge_data**: A data frame where each row represents an edge with
  columns:

  - `from`: Source node identifier

  - `to`: Target node identifier

  - `time`: Time period (character; "1" for cross-sectional networks)

  - `weight`: Edge weight values (using original weight variable name if
    specified)

  - Additional columns for any dyadic variables stored in the netify
    object

- **nodal_data**: A data frame where each row represents a node-time
  combination with columns:

  - `name`: Node identifier

  - `time`: Time period (character; "1" for cross-sectional networks)

  - Additional columns for any nodal variables stored in the netify
    object

## Details

The function helpful for:

**Edge data processing:**

- Extracts the adjacency matrix (or array for longitudinal networks)
  from the netify object

- Optionally removes zero-weight edges based on the remove_zeros
  parameter

- Merges any dyadic variables stored in the netify object

- Renames columns to standardized names (from, to, time)

**Node data processing:**

- Extracts nodal attributes if present, or constructs from actor_pds
  information

- Ensures consistent time variable across node and edge data

- Renames columns to standardized names (name, time)

**Time handling:**

- For cross-sectional networks: Sets time to "1" in both data frames

- For longitudinal networks: Preserves original time periods as
  character values

- For ego networks: Extracts time from ego-time concatenated identifiers

**Variable preservation:**

All dyadic and nodal variables stored in the netify object are preserved
in the output data frames. Dyadic variables are merged with the edge
data, while nodal variables remain in the nodal data frame.

## Author

Cassy Dorff, Shahryar Minhas

## Examples

``` r
# Load example data
data(icews)

# Example 1: Cross-sectional network
icews_10 <- icews[icews$year == 2010, ]

# Create netify object
net_cs <- netify(
    icews_10,
    actor1 = "i", actor2 = "j",
    symmetric = FALSE,
    weight = "verbCoop"
)

# Decompose to data frames
decomposed_cs <- decompose_netify(net_cs)

# Examine structure
str(decomposed_cs)
#> List of 2
#>  $ edge_data :'data.frame':  9976 obs. of  4 variables:
#>   ..$ from    : chr [1:9976] "Afghanistan" "Afghanistan" "Afghanistan" "Afghanistan" ...
#>   ..$ to      : chr [1:9976] "Argentina" "Armenia" "Australia" "Austria" ...
#>   ..$ time    : chr [1:9976] "1" "1" "1" "1" ...
#>   ..$ verbCoop: num [1:9976] 1 7 125 1 7 3 14 3 2 16 ...
#>  $ nodal_data:'data.frame':  152 obs. of  2 variables:
#>   ..$ name: chr [1:152] "Afghanistan" "Albania" "Algeria" "Angola" ...
#>   ..$ time: chr [1:152] "1" "1" "1" "1" ...
head(decomposed_cs$edge_data)
#>          from         to time verbCoop
#> 1 Afghanistan  Argentina    1        1
#> 2 Afghanistan    Armenia    1        7
#> 3 Afghanistan  Australia    1      125
#> 4 Afghanistan    Austria    1        1
#> 5 Afghanistan Azerbaijan    1        7
#> 6 Afghanistan    Bahrain    1        3
head(decomposed_cs$nodal_data)
#>          name time
#> 1 Afghanistan    1
#> 2     Albania    1
#> 3     Algeria    1
#> 4      Angola    1
#> 5   Argentina    1
#> 6     Armenia    1

# Example 2: Longitudinal network with attributes
nvars <- c("i_polity2", "i_log_gdp", "i_log_pop")
dvars <- c("matlCoop", "verbConf", "matlConf")

net_longit <- netify(
    icews,
    actor1 = "i", actor2 = "j",
    time = "year",
    symmetric = FALSE,
    weight = "verbCoop",
    nodal_vars = nvars,
    dyad_vars = dvars
)

# Decompose with all attributes
decomposed_longit <- decompose_netify(net_longit)

# Check that variables are preserved
names(decomposed_longit$edge_data) # Includes dyadic variables
#> [1] "from"     "to"       "time"     "verbCoop" "matlCoop" "verbConf" "matlConf"
names(decomposed_longit$nodal_data) # Includes nodal variables
#> [1] "name"      "time"      "i_polity2" "i_log_gdp" "i_log_pop"

# Example 3: Keep zero-weight edges
decomposed_with_zeros <- decompose_netify(net_cs, remove_zeros = FALSE)

# Compare edge counts
nrow(decomposed_cs$edge_data) # Without zeros
#> [1] 9976
nrow(decomposed_with_zeros$edge_data) # With zeros
#> [1] 22952

# Example 4: Use for visualization prep
if (FALSE) { # \dontrun{
# Decompose for use with ggplot2
plot_data <- decompose_netify(net_cs)

# Can now use edge_data and nodal_data separately
# for network visualization
} # }
```
