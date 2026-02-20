# Convert netify objects to statnet network format

Transforms netify network objects into statnet's network objects (also
available as `netify_to_network`, `to_statnet`, and `to_network`),
providing access to the extensive statistical modeling capabilities of
the statnet suite, including ERGMs (Exponential Random Graph Models),
descriptive statistics, and network visualization tools.

## Usage

``` r
netify_to_statnet(netlet, add_nodal_attribs = TRUE, add_dyad_attribs = TRUE)

netify_to_network(netlet, add_nodal_attribs = TRUE, add_dyad_attribs = TRUE)

to_statnet(netlet, add_nodal_attribs = TRUE, add_dyad_attribs = TRUE)

to_network(netlet, add_nodal_attribs = TRUE, add_dyad_attribs = TRUE)
```

## Arguments

- netlet:

  A netify object containing network data. Currently supports
  single-layer networks only. For multilayer networks, use
  [`subset_netify`](https://netify-dev.github.io/netify/reference/subset_netify.md)
  to extract individual layers first.

- add_nodal_attribs:

  Logical. If TRUE (default), includes nodal attributes from the netify
  object as vertex attributes in the network object. Set to FALSE to
  create a network with structure only.

- add_dyad_attribs:

  Logical. If TRUE (default), includes dyadic attributes from the netify
  object as edge attributes in the network object. Set to FALSE to
  exclude edge covariates.

## Value

A network object or list of network objects:

- Cross-sectional networks:

  Returns a single network object

- Longitudinal networks:

  Returns a named list of network objects, with names corresponding to
  time periods

The resulting network object(s) will have:

- Vertices named according to actors in the netify object

- Edge weights from the netify weight variable stored as "weight"
  attribute

- Vertex attributes for each nodal variable (if add_nodal_attribs =
  TRUE)

- Edge attributes for each dyadic variable (if add_dyad_attribs = TRUE)

- Proper directedness based on the symmetric parameter of the netify
  object

## Details

The conversion process handles different netify structures:

- **Cross-sectional**: Direct conversion to a single network object

- **Longitudinal arrays**: Internally converted to list format, then
  each time slice becomes a separate network object

- **Longitudinal lists**: Each time period converted to separate network
  object

The statnet network format stores networks as an edgelist with
attributes, making it memory-efficient for sparse networks. All nodal
and dyadic attributes from the netify object are preserved and can be
used in subsequent ERGM modeling or network analysis.

For longitudinal data, each time period results in an independent
network object. This format is suitable for discrete-time network
analysis or pooled ERGM estimation across time periods.

## Note

This function requires the network package (part of statnet) to be
installed.

For ERGM modeling, the ergm package (also part of statnet) should be
loaded after creating the network objects.

## Author

Ha Eun Choi, Cassy Dorff, Colin Henry, Shahryar Minhas

## Examples

``` r
# Load example data
data(icews)

# Cross-sectional example
icews_10 <- icews[icews$year == 2010, ]

# Create netify object with attributes
dvars <- c("matlCoop", "verbConf", "matlConf")
nvars <- c("i_polity2", "i_log_gdp", "i_log_pop")

verbCoop_net <- netify(
    icews_10,
    actor1 = "i", actor2 = "j",
    symmetric = FALSE,
    weight = "verbCoop",
    dyad_vars = dvars,
    dyad_vars_symmetric = rep(FALSE, length(dvars)),
    nodal_vars = nvars
)

# Convert to statnet network object
ntwk <- netify_to_statnet(verbCoop_net)

# Examine the result
ntwk
#>  Network attributes:
#>   vertices = 152 
#>   directed = TRUE 
#>   hyper = FALSE 
#>   loops = FALSE 
#>   multiple = FALSE 
#>   bipartite = FALSE 
#>   verbCoop: 152x152 matrix
#>   matlCoop: 152x152 matrix
#>   verbConf: 152x152 matrix
#>   matlConf: 152x152 matrix
#>   total edges= 9976 
#>     missing edges= 0 
#>     non-missing edges= 9976 
#> 
#>  Vertex attribute names: 
#>     i_log_gdp i_log_pop i_polity2 vertex.names 
#> 
#>  Edge attribute names not shown 
network::network.size(ntwk) # number of vertices
#> [1] 152
network::network.edgecount(ntwk) # number of edges
#> [1] 9976
network::list.vertex.attributes(ntwk) # nodal attributes
#> [1] "i_log_gdp"    "i_log_pop"    "i_polity2"    "na"           "vertex.names"
network::list.edge.attributes(ntwk) # edge attributes
#> [1] "matlConf_e" "matlCoop_e" "na"         "verbConf_e" "verbCoop"  

# Access specific attributes
network::get.vertex.attribute(ntwk, "i_polity2") # polity scores
#>   [1]  NA   9   2  -2   8   5  10  10  -7  -5   5  -7   8   7   7  NA   8   9
#>  [19]   0   6   2  -4  10  10  -1  -2  10  -7   7   9  -4   5  10   0   9  -7
#>  [37]  10  10   2   8   5  -3   8  -6   9  -3  -4  10  10   3  -5   6  10   8
#>  [55]  10   8   1   6   6   0   7  10   9   8  -7   3  10   6  10   9  10  -3
#>  [73]  -6   8 -10   8  -7   4  -7   8   6   6  -7  10  10   9   0   6   6   7
#>  [91]  -2  10   8   9  10  -6   5  -6   6  10  10   9   3   4  10  -8   6   9
#> [109]   4   8   9   8  10  10 -10   9   4  -4 -10   7   7  -2  10  10   8   0
#> [127]   9  10   3  -2   5  10  10  -7  -3  -1   4  -2  10  -4  -9  -1   6  -8
#> [145]  10  10  10  -9  -7  -2   7   1
network::get.edge.attribute(ntwk, "matlCoop") # material cooperation
#> NULL

# Check network properties
network::is.directed(ntwk) # TRUE for this example
#> [1] TRUE
network::has.loops(ntwk) # FALSE (no self-ties)
#> [1] FALSE

# \donttest{
# Longitudinal example
verbCoop_longit <- netify(
    icews,
    actor1 = "i", actor2 = "j", time = "year",
    symmetric = FALSE,
    weight = "verbCoop",
    dyad_vars = dvars,
    dyad_vars_symmetric = rep(FALSE, length(dvars)),
    nodal_vars = nvars
)

# Convert to list of network objects
ntwk_list <- netify_to_statnet(verbCoop_longit)

# Examine structure
length(ntwk_list) # number of time periods
#> [1] 13
names(ntwk_list) # time period labels
#>  [1] "2002" "2003" "2004" "2005" "2006" "2007" "2008" "2009" "2010" "2011"
#> [11] "2012" "2013" "2014"

# Access specific time period
ntwk_2002 <- ntwk_list[["2002"]]
ntwk_2002
#>  Network attributes:
#>   vertices = 152 
#>   directed = TRUE 
#>   hyper = FALSE 
#>   loops = FALSE 
#>   multiple = FALSE 
#>   bipartite = FALSE 
#>   verbCoop: 152x152 matrix
#>   matlCoop: 152x152 matrix
#>   verbConf: 152x152 matrix
#>   matlConf: 152x152 matrix
#>   total edges= 8692 
#>     missing edges= 0 
#>     non-missing edges= 8692 
#> 
#>  Vertex attribute names: 
#>     i_log_gdp i_log_pop i_polity2 vertex.names 
#> 
#>  Edge attribute names not shown 
# }

if (FALSE) { # \dontrun{
# Use with ergm for modeling (requires ergm package)
library(ergm)
model <- ergm(ntwk ~ edges + mutual + nodematch("i_polity2"))
} # }
```
