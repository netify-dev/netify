# Add dyadic variables to a netify object

`add_dyad_vars` (also available as `add_edge_attributes`) merges
additional dyadic (edge-level) variables from a data.frame into an
existing netify object. this function allows you to incrementally build
up the dyadic attributes of your network after initial creation, which
is useful when variables come from different sources or need different
preprocessing.

## Usage

``` r
add_dyad_vars(
  netlet,
  dyad_data,
  actor1 = NULL,
  actor2 = NULL,
  time = NULL,
  dyad_vars = NULL,
  dyad_vars_symmetric = NULL,
  replace_existing = FALSE
)

add_edge_attributes(
  netlet,
  dyad_data,
  actor1 = NULL,
  actor2 = NULL,
  time = NULL,
  dyad_vars = NULL,
  dyad_vars_symmetric = NULL,
  replace_existing = FALSE
)
```

## Arguments

- netlet:

  a netify object (class "netify") to which dyadic variables will be
  added.

- dyad_data:

  a data.frame object containing the dyadic variables to add. must
  include columns matching the actor1, actor2, and time specifications
  used in the original netify object. will be coerced to data.frame if a
  tibble or data.table is provided.

- actor1:

  character string specifying the column name in dyad_data for the first
  actor in each dyad. should match the actor1 specification used when
  creating the netify object.

- actor2:

  character string specifying the column name in dyad_data for the
  second actor in each dyad. should match the actor2 specification used
  when creating the netify object.

- time:

  character string specifying the column name in dyad_data for time
  periods. required for longitudinal netify objects. should match the
  time specification used when creating the netify object. set to NULL
  for cross-sectional networks.

- dyad_vars:

  character vector of column names from dyad_data to add as dyadic
  variables. if NULL (default), all columns except actor1, actor2, and
  time will be added.

- dyad_vars_symmetric:

  logical vector indicating whether each dyadic variable represents
  symmetric relationships. must have the same length as dyad_vars. if
  NULL, defaults to the symmetry setting of the netify object, but a
  warning will be issued recommending explicit specification.

- replace_existing:

  logical scalar. if TRUE, existing dyadic variables with the same names
  will be replaced. if FALSE (default), attempting to add variables that
  already exist will result in an error.

## Value

a netify object (class "netify") with the additional dyadic variables
stored in the 'dyad_data' attribute. the structure is a nested list
where:

- first level: named list with time periods as names (or "1" for
  cross-sectional data)

- second level: named list with variable names as names

- values: matrix objects with actors as rows/columns and numeric,
  integer, logical, or character values

## Details

dyadic variables are stored as matrix objects where rows represent the
first actor (sender in directed networks) and columns represent the
second actor (receiver in directed networks). for symmetric variables in
undirected networks, the function ensures that `matrix[i,j]` equals
`matrix[j,i]`.

the function optimizes storage by automatically detecting the data type
of each variable and using the appropriate matrix storage mode:

- logical vectors -\> logical matrices

- integer vectors -\> integer matrices

- numeric vectors with only integer values -\> integer matrices

- numeric vectors with decimals -\> double matrices

- character vectors -\> character matrices

for longitudinal networks, the function handles time-varying actor sets
appropriately, creating matrices that include only actors present at
each time point.

missing dyadic observations (na values) in the input data.frame will be
set to missing in the resulting matrices as well.

## Note

the input `dyad_data` must be a `data.frame` or an object that can be
coerced into a `data.frame` (e.g., a `tibble` or `data.table`). inputs
such as matrices or arrays are not supported.

when adding dyadic variables to bipartite networks, all variables are
automatically treated as asymmetric regardless of the
dyad_vars_symmetric specification.

for large networks, consider the memory implications of adding many
dyadic variables, as each variable requires a full adjacency matrix for
storage.

## Author

cassy dorff, colin henry, shahryar minhas

cassy dorff, shahryar minhas

## Examples

``` r
# load example data
data(icews)

# cross-sectional example
icews_10 <- icews[icews$year == 2010, ]
actors <- sort(unique(c(icews_10$i, icews_10$j)))[1:35]
icews_10 <- icews_10[icews_10$i %in% actors & icews_10$j %in% actors, ]

# create initial netify object with just the main weight
verbCoop_net <- netify(
    icews_10, # data.frame input
    actor1 = "i", actor2 = "j",
    symmetric = FALSE,
    weight = "verbCoop"
)
#> ℹ `missing_to_zero` is set to "TRUE" (the default).
#> ! Missing dyads will be filled with zeros. For latent space or other
#>   statistical network models, structural zeros and missing data have different
#>   meanings. Set `missing_to_zero = FALSE` to preserve NAs if this distinction
#>   matters for your analysis.
#> This message is displayed once per session.

# check class
class(verbCoop_net) # "netify"
#> [1] "netify"

# add additional dyadic variables
verbCoop_net <- add_dyad_vars(
    netlet = verbCoop_net, # netify object
    dyad_data = icews_10, # data.frame with variables to add
    actor1 = "i", actor2 = "j",
    dyad_vars = c("matlCoop", "verbConf", "matlConf"),
    dyad_vars_symmetric = rep(FALSE, 3)
)

# access the dyadic data structure (returns list)
dyad_data_structure <- attr(verbCoop_net, "dyad_data")
class(dyad_data_structure) # "list"
#> [1] "list"
names(dyad_data_structure) # time periods
#> [1] "1"
names(dyad_data_structure[["1"]]) # variables at time 1
#> [1] "matlCoop" "verbConf" "matlConf"

# access specific variable matrix
matlCoop_matrix <- dyad_data_structure[["1"]][["matlCoop"]]
class(matlCoop_matrix) # "matrix" "array"
#> [1] "matrix" "array" 
dim(matlCoop_matrix)
#> [1] 35 35
matlCoop_matrix[1:5, 1:5] # view subset
#>             Afghanistan Albania Algeria Angola Argentina
#> Afghanistan           0       1       0      0         0
#> Albania               4       0       0      0         0
#> Algeria               0       0       0      2         0
#> Angola                0       0       0      0         0
#> Argentina             0       0       1      0         0

# longitudinal example
icews_panel <- icews[
    icews$year %in% 2002:2004 &
        icews$i %in% actors &
        icews$j %in% actors,
]

verbCoop_longit_net <- netify(
    icews_panel, # data.frame input
    actor1 = "i", actor2 = "j", time = "year",
    symmetric = FALSE,
    weight = "verbCoop"
)

# add dyadic variables across all time periods
verbCoop_longit_net <- add_dyad_vars(
    netlet = verbCoop_longit_net, # netify object
    dyad_data = icews_panel, # data.frame with longitudinal data
    actor1 = "i", actor2 = "j", time = "year",
    dyad_vars = c("matlCoop", "verbConf", "matlConf"),
    dyad_vars_symmetric = rep(FALSE, 3)
)

# access data for specific year (returns list)
year_2002_data <- attr(verbCoop_longit_net, "dyad_data")[["2002"]]
class(year_2002_data) # "list"
#> [1] "list"
names(year_2002_data) # available variables
#> [1] "matlCoop" "verbConf" "matlConf"

# each variable is stored as a matrix
matlCoop_2002 <- year_2002_data[["matlCoop"]]
class(matlCoop_2002) # "matrix" "array"
#> [1] "matrix" "array" 

# example: add variables from a different source
# \donttest{
# create a new data.frame with trade information
trade_data <- data.frame(
    i = icews_10$i,
    j = icews_10$j,
    trade_volume = runif(nrow(icews_10), 0, 1000),
    trade_balance = rnorm(nrow(icews_10))
)
class(trade_data) # "data.frame"
#> [1] "data.frame"

verbCoop_net <- add_dyad_vars(
    netlet = verbCoop_net,
    dyad_data = trade_data,
    actor1 = "i", actor2 = "j",
    dyad_vars = c("trade_volume", "trade_balance"),
    dyad_vars_symmetric = c(FALSE, FALSE)
)
# }
```
