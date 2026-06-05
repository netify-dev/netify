# Add nodal variables to a netify object

`add_node_vars` (also available as `add_vertex_attributes`) merges nodal
(vertex-level) variables from a data.frame into an existing netify
object. this function allows you to incrementally build up the nodal
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

  a netify object (class "netify") to which nodal variables will be
  added.

- node_data:

  a data.frame object containing the nodal variables to add. for
  cross-sectional networks, must have one row per unique actor. for
  longitudinal networks, must have one row per actor-time combination.
  will be coerced to data.frame if a tibble or data.table is provided.

- actor:

  character string specifying the column name in node_data that uniquely
  identifies each actor. this should contain the same actor identifiers
  used in the original netify object.

- time:

  character string specifying the column name in node_data for time
  periods. required for longitudinal netify objects. should match the
  time specification used when creating the netify object. set to NULL
  for cross-sectional networks.

- node_vars:

  character vector of column names from node_data to add as nodal
  variables. if NULL (default), all columns except actor and time will
  be added.

- replace_existing:

  logical scalar. if TRUE, existing nodal variables with the same names
  will be replaced. if FALSE (default), attempting to add variables that
  already exist will result in an error.

## Value

a netify object (class "netify") with the additional nodal variables
stored in the 'nodal_data' attribute as a data.frame. the structure
includes:

- **actor**: column with actor identifiers

- **time**: column with time periods (longitudinal only)

- **nodal variables**: columns for each variable specified in node_vars

## Details

nodal variables are stored as a data.frame where each row represents an
actor (cross-sectional) or an actor-time combination (longitudinal).
this format allows for efficient storage and easy manipulation of
actor-level attributes.

the function automatically handles merging based on actor identifiers,
ensuring that nodal attributes are properly aligned with the network
structure. for longitudinal networks, the function matches both actor
and time dimensions.

missing actors in the node_data will result in na values for those
actors' attributes in the netify object. similarly, if node_data
contains actors not present in the network, those rows will be ignored.

## Note

the input `node_data` must be a `data.frame` or an object that can be
coerced into a `data.frame` (e.g., a `tibble` or `data.table`). inputs
such as matrices or arrays are not supported.

for longitudinal networks, ensure that node_data contains entries for
all actor-time combinations you wish to have attributes for. missing
combinations will result in na values for those actors at those time
points.

when working with large networks, the nodal data storage is more
memory-efficient than dyadic data, as it scales linearly with the number
of actors rather than quadratically.

## Author

colin henry, shahryar minhas

cassy dorff, shahryar minhas

## Examples

``` r
# load example data
data(icews)

# cross-sectional example
icews_10 <- icews[icews$year == 2010, ]

# create initial netify object
verbCoop_net <- netify(
    icews_10, # data.frame input
    actor1 = "i", actor2 = "j",
    symmetric = FALSE,
    weight = "verbCoop"
)

# prepare nodal data - one row per unique actor
nvars <- c("i_polity2", "i_gdp", "i_log_gdp", "i_pop", "i_log_pop")
nodedata <- unique(icews_10[, c("i", nvars)])
class(nodedata) # "data.frame"
#> [1] "data.frame"
nrow(nodedata) # number of unique actors
#> [1] 152
head(nodedata)
#>                 i i_polity2        i_gdp i_log_gdp    i_pop i_log_pop
#> 10    Afghanistan        NA  16047892927  23.49884 28189672  17.15447
#> 2250      Albania         9  10420206418  23.06701  2913021  14.88470
#> 4490      Algeria         2 140977153156  25.67186 35856344  17.39503
#> 6730       Angola        -2  69938841426  24.97089 23364185  16.96671
#> 8970    Argentina         8 552738161802  27.03815 40788453  17.52391
#> 11210     Armenia         5   8513508876  22.86492  2946293  14.89606

# add nodal variables
verbCoop_net <- add_node_vars(
    netlet = verbCoop_net, # netify object
    node_data = nodedata, # data.frame with actor attributes
    actor = "i", # column identifying actors
    node_vars = nvars # variables to add
)

# access nodal data (returns data.frame)
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

# \donttest{
# longitudinal example
verbCoop_longit_net <- netify(
    icews, # data.frame input
    actor1 = "i", actor2 = "j", time = "year",
    symmetric = FALSE,
    weight = "verbCoop"
)

# prepare longitudinal nodal data - one row per actor-time combination
nodedata_longit <- unique(icews[, c("i", "year", nvars)])

# add nodal variables with time dimension
verbCoop_longit_net <- add_node_vars(
    netlet = verbCoop_longit_net, # netify object
    node_data = nodedata_longit, # data.frame with longitudinal data
    actor = "i", # column identifying actors
    time = "year", # column identifying time
    node_vars = nvars # variables to add
)
# }

# \donttest{
# add variables from external source
# suppose you have additional actor data
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
# }
```
