# Add nodal variables to a netify object

`add_node_vars` (also available as `add_vertex_attributes`) merges nodal
(vertex-level) variables from a data.frame into an existing netify
object. This function allows you to incrementally build up the nodal
attributes of your network after initial creation, which is useful when
actor-level variables come from different sources or need different
preprocessing.

## Usage

``` r
add_node_vars(
  netlet,
  node_data,
  actor = NULL,
  time = NULL,
  node_vars = NULL,
  replace_existing = FALSE
)

add_vertex_attributes(
  netlet,
  node_data,
  actor = NULL,
  time = NULL,
  node_vars = NULL,
  replace_existing = FALSE
)
```

## Arguments

- netlet:

  A netify object (class "netify") to which nodal variables will be
  added.

- node_data:

  A data.frame object containing the nodal variables to add. For
  cross-sectional networks, must have one row per unique actor. For
  longitudinal networks, must have one row per actor-time combination.
  Will be coerced to data.frame if a tibble or data.table is provided.

- actor:

  Character string specifying the column name in node_data that uniquely
  identifies each actor. This should contain the same actor identifiers
  used in the original netify object.

- time:

  Character string specifying the column name in node_data for time
  periods. Required for longitudinal netify objects. Should match the
  time specification used when creating the netify object. Set to NULL
  for cross-sectional networks.

- node_vars:

  Character vector of column names from node_data to add as nodal
  variables. If NULL (default), all columns except actor and time will
  be added.

- replace_existing:

  Logical scalar. If TRUE, existing nodal variables with the same names
  will be replaced. If FALSE (default), attempting to add variables that
  already exist will result in an error.

## Value

A netify object (class "netify") with the additional nodal variables
stored in the 'nodal_data' attribute as a data.frame. The structure
includes:

- **actor**: Column with actor identifiers

- **time**: Column with time periods (longitudinal only)

- **nodal variables**: Columns for each variable specified in node_vars

## Details

Nodal variables are stored as a data.frame where each row represents an
actor (cross-sectional) or an actor-time combination (longitudinal).
This format allows for efficient storage and easy manipulation of
actor-level attributes.

The function automatically handles merging based on actor identifiers,
ensuring that nodal attributes are properly aligned with the network
structure. For longitudinal networks, the function matches both actor
and time dimensions.

Missing actors in the node_data will result in NA values for those
actors' attributes in the netify object. Similarly, if node_data
contains actors not present in the network, those rows will be ignored.

## Note

The input `node_data` must be a `data.frame` or an object that can be
coerced into a `data.frame` (e.g., a `tibble` or `data.table`). Inputs
such as matrices or arrays are not supported.

For longitudinal networks, ensure that node_data contains entries for
all actor-time combinations you wish to have attributes for. Missing
combinations will result in NA values for those actors at those time
points.

When working with large networks, the nodal data storage is more
memory-efficient than dyadic data, as it scales linearly with the number
of actors rather than quadratically.

## Author

Colin Henry, Shahryar Minhas

## Examples

``` r
# Load example data
data(icews)

# Cross-sectional example
icews_10 <- icews[icews$year == 2010, ]

# Create initial netify object
verbCoop_net <- netify(
    icews_10, # data.frame input
    actor1 = "i", actor2 = "j",
    symmetric = FALSE,
    weight = "verbCoop"
)

# Prepare nodal data - one row per unique actor
nvars <- c("i_polity2", "i_gdp", "i_log_gdp", "i_pop", "i_log_pop")
nodeData <- unique(icews_10[, c("i", nvars)])
class(nodeData) # "data.frame"
#> [1] "data.frame"
nrow(nodeData) # Number of unique actors
#> [1] 152
head(nodeData)
#>                 i i_polity2        i_gdp i_log_gdp    i_pop i_log_pop
#> 10    Afghanistan        NA  16047892927  23.49884 28189672  17.15447
#> 2250      Albania         9  10420206418  23.06701  2913021  14.88470
#> 4490      Algeria         2 140977153156  25.67186 35856344  17.39503
#> 6730       Angola        -2  69938841426  24.97089 23364185  16.96671
#> 8970    Argentina         8 552738161802  27.03815 40788453  17.52391
#> 11210     Armenia         5   8513508876  22.86492  2946293  14.89606

# Add nodal variables
verbCoop_net <- add_node_vars(
    netlet = verbCoop_net, # netify object
    node_data = nodeData, # data.frame with actor attributes
    actor = "i", # column identifying actors
    node_vars = nvars # variables to add
)

# Access nodal data (returns data.frame)
node_data_stored <- attr(verbCoop_net, "nodal_data")
class(node_data_stored) # "data.frame"
#> [1] "data.frame"
head(node_data_stored)
#>         actor i_polity2        i_gdp i_log_gdp    i_pop i_log_pop
#> 1 Afghanistan        NA  16047892927  23.49884 28189672  17.15447
#> 2     Albania         9  10420206418  23.06701  2913021  14.88470
#> 3     Algeria         2 140977153156  25.67186 35856344  17.39503
#> 4      Angola        -2  69938841426  24.97089 23364185  16.96671
#> 5   Argentina         8 552738161802  27.03815 40788453  17.52391
#> 6     Armenia         5   8513508876  22.86492  2946293  14.89606
names(node_data_stored) # "actor" plus variable names
#> [1] "actor"     "i_polity2" "i_gdp"     "i_log_gdp" "i_pop"     "i_log_pop"

# Longitudinal example
verbCoop_longit_net <- netify(
    icews, # data.frame input
    actor1 = "i", actor2 = "j", time = "year",
    symmetric = FALSE,
    weight = "verbCoop"
)

# Prepare longitudinal nodal data - one row per actor-time combination
nodeData_longit <- unique(icews[, c("i", "year", nvars)])
class(nodeData_longit) # "data.frame"
#> [1] "data.frame"
nrow(nodeData_longit) # Number of actor-time combinations
#> [1] 1976

# Add nodal variables with time dimension
verbCoop_longit_net <- add_node_vars(
    netlet = verbCoop_longit_net, # netify object
    node_data = nodeData_longit, # data.frame with longitudinal data
    actor = "i", # column identifying actors
    time = "year", # column identifying time
    node_vars = nvars # variables to add
)

# Access longitudinal nodal data
node_data_longit <- attr(verbCoop_longit_net, "nodal_data")
class(node_data_longit) # "data.frame"
#> [1] "data.frame"
head(node_data_longit) # Now includes time column
#>         actor time i_polity2       i_gdp i_log_gdp    i_pop i_log_pop
#> 1 Afghanistan 2002        NA  7555185296  22.74550 21000256  16.86005
#> 2 Afghanistan 2003        NA  8222480251  22.83014 22645130  16.93546
#> 3 Afghanistan 2004        NA  8338755823  22.84418 23553551  16.97479
#> 4 Afghanistan 2005        NA  9275174321  22.95061 24411191  17.01055
#> 5 Afghanistan 2006        NA  9772082812  23.00280 25442944  17.05195
#> 6 Afghanistan 2007        NA 11123202208  23.13230 25903301  17.06988

# Filter to specific time period
node_data_2010 <- node_data_longit[node_data_longit$time == "2010", ]
nrow(node_data_2010) # Number of actors in 2010
#> [1] 152

# Example: Add variables from external source
if (FALSE) { # \dontrun{
# Suppose you have additional actor data
external_data <- data.frame(
    i = unique(icews_10$i),
    democracy_score = runif(length(unique(icews_10$i)), 0, 10),
    trade_openness = runif(length(unique(icews_10$i)), 0, 100)
)

verbCoop_net <- add_node_vars(
    netlet = verbCoop_net,
    node_data = external_data,
    actor = "i",
    node_vars = c("democracy_score", "trade_openness")
)
} # }
```
