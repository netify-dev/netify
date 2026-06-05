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

  a netify object (class "netify") to be decomposed.

- remove_zeros:

  logical. if TRUE (default), edges with zero weight values are removed
  from the edge data frame. if FALSE, zero-weight edges are retained.

## Value

a list containing two data frames:

- **edge_data**: a data frame where each row represents an edge with
  columns:

  - `from`: source node identifier

  - `to`: target node identifier

  - `time`: time period (character; "1" for cross-sectional networks)

  - `weight`: edge weight values (using original weight variable name if
    specified)

  - additional columns for any dyadic variables stored in the netify
    object

- **nodal_data**: a data frame where each row represents a node-time
  combination with columns:

  - `name`: node identifier

  - `time`: time period (character; "1" for cross-sectional networks)

  - additional columns for any nodal variables stored in the netify
    object

## Details

the function helpful for:

**edge data processing:**

- extracts the adjacency matrix (or array for longitudinal networks)
  from the netify object

- optionally removes zero-weight edges based on the remove_zeros
  parameter

- merges any dyadic variables stored in the netify object

- renames columns to standardized names (from, to, time)

**node data processing:**

- extracts nodal attributes if present, or constructs from actor_pds
  information

- ensures consistent time variable across node and edge data

- renames columns to standardized names (name, time)

**time handling:**

- for cross-sectional networks: sets time to "1" in both data frames

- for longitudinal networks: preserves original time periods as
  character values

- for ego networks: extracts time from ego-time concatenated identifiers

**variable preservation:**

all dyadic and nodal variables stored in the netify object are preserved
in the output data frames. dyadic variables are merged with the edge
data, while nodal variables remain in the nodal data frame.

## Author

cassy dorff, shahryar minhas

## Examples

``` r
# load example data
data(icews)

# example 1: cross-sectional network
icews_10 <- icews[icews$year == 2010, ]

# create netify object
net_cs <- netify(
    icews_10,
    actor1 = "i", actor2 = "j",
    symmetric = FALSE,
    weight = "verbCoop"
)

# decompose to data frames
decomposed_cs <- decompose_netify(net_cs)

# examine structure
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

# example 2: keep zero-weight edges
decomposed_with_zeros <- decompose_netify(net_cs, remove_zeros = FALSE)

# compare edge counts
nrow(decomposed_cs$edge_data) # without zeros
#> [1] 9976
nrow(decomposed_with_zeros$edge_data) # with zeros
#> [1] 22952

# example 4: use for visualization prep
if (FALSE) { # \dontrun{
# decompose for use with ggplot2
plot_data <- decompose_netify(net_cs)

# can now use edge_data and nodal_data separately
# for network visualization
} # }
```
