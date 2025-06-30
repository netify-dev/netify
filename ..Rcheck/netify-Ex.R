pkgname <- "netify"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('netify')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("add_dyad_vars")
### * add_dyad_vars

flush(stderr()); flush(stdout())

### Name: add_dyad_vars
### Title: Add dyadic variables to a netify object
### Aliases: add_dyad_vars add_edge_attributes

### ** Examples

# Load example data
data(icews)

# Cross-sectional example
icews_10 <- icews[icews$year == 2010, ]

# Create initial netify object with just the main weight
verbCoop_net <- netify(
    icews_10, # data.frame input
    actor1 = "i", actor2 = "j",
    symmetric = FALSE,
    weight = "verbCoop"
)

# Check class
class(verbCoop_net) # "netify"

# Add additional dyadic variables
verbCoop_net <- add_dyad_vars(
    netlet = verbCoop_net, # netify object
    dyad_data = icews_10, # data.frame with variables to add
    actor1 = "i", actor2 = "j",
    dyad_vars = c("matlCoop", "verbConf", "matlConf"),
    dyad_vars_symmetric = rep(FALSE, 3)
)

# Access the dyadic data structure (returns list)
dyad_data_structure <- attr(verbCoop_net, "dyad_data")
class(dyad_data_structure) # "list"
names(dyad_data_structure) # Time periods
names(dyad_data_structure[["1"]]) # Variables at time 1

# Access specific variable matrix
matlCoop_matrix <- dyad_data_structure[["1"]][["matlCoop"]]
class(matlCoop_matrix) # "matrix" "array"
dim(matlCoop_matrix)
matlCoop_matrix[1:5, 1:5] # View subset

# Longitudinal example
verbCoop_longit_net <- netify(
    icews, # data.frame input
    actor1 = "i", actor2 = "j", time = "year",
    symmetric = FALSE,
    weight = "verbCoop"
)

# Add dyadic variables across all time periods
verbCoop_longit_net <- add_dyad_vars(
    netlet = verbCoop_longit_net, # netify object
    dyad_data = icews, # data.frame with longitudinal data
    actor1 = "i", actor2 = "j", time = "year",
    dyad_vars = c("matlCoop", "verbConf", "matlConf"),
    dyad_vars_symmetric = rep(FALSE, 3)
)

# Access data for specific year (returns list)
year_2002_data <- attr(verbCoop_longit_net, "dyad_data")[["2002"]]
class(year_2002_data) # "list"
names(year_2002_data) # Available variables

# Each variable is stored as a matrix
matlCoop_2002 <- year_2002_data[["matlCoop"]]
class(matlCoop_2002) # "matrix" "array"

# Example: Add variables from a different source
## Not run: 
##D # Create a new data.frame with trade information
##D trade_data <- data.frame(
##D     i = icews_10$i,
##D     j = icews_10$j,
##D     trade_volume = runif(nrow(icews_10), 0, 1000),
##D     trade_balance = rnorm(nrow(icews_10))
##D )
##D class(trade_data) # "data.frame"
##D 
##D verbCoop_net <- add_dyad_vars(
##D     netlet = verbCoop_net,
##D     dyad_data = trade_data,
##D     actor1 = "i", actor2 = "j",
##D     dyad_vars = c("trade_volume", "trade_balance"),
##D     dyad_vars_symmetric = c(FALSE, FALSE)
##D )
## End(Not run)




cleanEx()
nameEx("add_node_vars")
### * add_node_vars

flush(stderr()); flush(stdout())

### Name: add_node_vars
### Title: Add nodal variables to a netify object
### Aliases: add_node_vars add_vertex_attributes

### ** Examples

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
nrow(nodeData) # Number of unique actors
head(nodeData)

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
head(node_data_stored)
names(node_data_stored) # "actor" plus variable names

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
nrow(nodeData_longit) # Number of actor-time combinations

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
head(node_data_longit) # Now includes time column

# Filter to specific time period
node_data_2010 <- node_data_longit[node_data_longit$time == "2010", ]
nrow(node_data_2010) # Number of actors in 2010

# Example: Add variables from external source
## Not run: 
##D # Suppose you have additional actor data
##D external_data <- data.frame(
##D     i = unique(icews_10$i),
##D     democracy_score = runif(length(unique(icews_10$i)), 0, 10),
##D     trade_openness = runif(length(unique(icews_10$i)), 0, 100)
##D )
##D 
##D verbCoop_net <- add_node_vars(
##D     netlet = verbCoop_net,
##D     node_data = external_data,
##D     actor = "i",
##D     node_vars = c("democracy_score", "trade_openness")
##D )
## End(Not run)




cleanEx()
nameEx("aggregate_dyad")
### * aggregate_dyad

flush(stderr()); flush(stdout())

### Name: aggregate_dyad
### Title: Aggregate dyadic event data by actor pairs
### Aliases: aggregate_dyad

### ** Examples

# Load example data
data(icews)

# Example 1: Aggregate multiple events between countries
# The icews data contains multiple events per country pair
icews_2010 <- icews[icews$year == 2010, ]

# Aggregate directed cooperation events
agg_coop <- aggregate_dyad(
    dyad_data = icews_2010,
    actor1 = "i",
    actor2 = "j",
    weight = "verbCoop",
    symmetric = FALSE
)

# Check reduction in observations
nrow(icews_2010) # Original observations
nrow(agg_coop) # Unique directed dyads

# Example 2: Create symmetric trade volumes
trade_data <- data.frame(
    exporter = c("USA", "USA", "China", "China", "USA", "China"),
    importer = c("China", "China", "USA", "USA", "UK", "UK"),
    year = c(2020, 2020, 2020, 2021, 2021, 2021),
    trade_value = c(100, 50, 75, 80, 120, 90)
)

# Aggregate as total trade between countries (undirected)
total_trade <- aggregate_dyad(
    dyad_data = trade_data,
    actor1 = "exporter",
    actor2 = "importer",
    time = "year",
    weight = "trade_value",
    symmetric = TRUE
)

# USA-China trade in 2020: 100+50+75 = 225 (appears in both directions)
total_trade[total_trade$year == 2020, ]

# Example 3: Aggregate across all time periods
all_time_trade <- aggregate_dyad(
    dyad_data = trade_data,
    actor1 = "exporter",
    actor2 = "importer",
    time = NULL, # Aggregate across all years
    weight = "trade_value",
    symmetric = FALSE
)

# USA total exports to China: 100+50 = 150
all_time_trade

# Example 4: Handle missing values
trade_data_na <- trade_data
trade_data_na$trade_value[2] <- NA

# Ignore missing values (default)
agg_ignore_na <- aggregate_dyad(
    dyad_data = trade_data_na,
    actor1 = "exporter",
    actor2 = "importer",
    time = "year",
    weight = "trade_value",
    symmetric = FALSE,
    ignore_missing = TRUE
)

# Include missing values
agg_with_na <- aggregate_dyad(
    dyad_data = trade_data_na,
    actor1 = "exporter",
    actor2 = "importer",
    time = "year",
    weight = "trade_value",
    symmetric = FALSE,
    ignore_missing = FALSE
)

# Compare results for USA->China in 2020
agg_ignore_na[agg_ignore_na$exporter == "USA" &
    agg_ignore_na$importer == "China" &
    agg_ignore_na$year == 2020, ] # 100 (ignored NA)

agg_with_na[agg_with_na$exporter == "USA" &
    agg_with_na$importer == "China" &
    agg_with_na$year == 2020, ] # NA




cleanEx()
nameEx("assemble_netify_plot")
### * assemble_netify_plot

flush(stderr()); flush(stdout())

### Name: assemble_netify_plot
### Title: Assemble netify plot from components
### Aliases: assemble_netify_plot

### ** Examples

## Not run: 
##D # create a netify object
##D net <- netify(my_data, actor1 = "from", actor2 = "to")
##D 
##D # get plot components
##D comp <- plot(net, return_components = TRUE)
##D 
##D # reassemble the plot
##D p <- assemble_netify_plot(comp)
##D print(p)
## End(Not run)




cleanEx()
nameEx("compare_networks")
### * compare_networks

flush(stderr()); flush(stdout())

### Name: compare_networks
### Title: Compare networks across time, layers, or attributes
### Aliases: compare_networks

### ** Examples

# Load example data
data(icews)

# Create networks for different years
net_2002 <- netify(icews[icews$year == 2002, ],
    actor1 = "i", actor2 = "j",
    weight = "matlConf"
)
net_2003 <- netify(icews[icews$year == 2003, ],
    actor1 = "i", actor2 = "j",
    weight = "matlConf"
)

# Basic edge comparison
comp <- compare_networks(list("2002" = net_2002, "2003" = net_2003))
print(comp)

# Structural comparison
struct_comp <- compare_networks(
    list(net_2002, net_2003),
    what = "structure"
)

# Create longitudinal network for automatic temporal comparison
longit_net <- netify(
    icews,
    actor1 = "i", actor2 = "j",
    time = "year",
    weight = "verbCoop",
    output_format = "longit_list"
)

# Automatic temporal comparison
temporal_comp <- compare_networks(longit_net, method = "all")

# Create multilayer network example
## Not run: 
##D # Create separate networks for different interaction types
##D verbal_coop <- netify(
##D     icews[icews$year == 2010, ],
##D     actor1 = "i", actor2 = "j",
##D     weight = "verbCoop"
##D )
##D 
##D material_coop <- netify(
##D     icews[icews$year == 2010, ],
##D     actor1 = "i", actor2 = "j",
##D     weight = "matlCoop"
##D )
##D 
##D # Combine into multilayer network
##D multilayer <- layer_netify(
##D     list(verbal = verbal_coop, material = material_coop)
##D )
##D 
##D # Automatic multilayer comparison
##D layer_comp <- compare_networks(multilayer, method = "all")
##D print(layer_comp)
##D # Will show comparison between verbal and material cooperation layers
## End(Not run)

# Get detailed matrices
detailed_comp <- compare_networks(
    list(net_2002, net_2003),
    return_details = TRUE
)
names(detailed_comp$details) # Shows available matrices




cleanEx()
nameEx("decompose_netify")
### * decompose_netify

flush(stderr()); flush(stdout())

### Name: decompose_netify
### Title: Decompose a netify object into edge and node data frames
### Aliases: decompose_netify

### ** Examples

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
head(decomposed_cs$edge_data)
head(decomposed_cs$nodal_data)

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
names(decomposed_longit$nodal_data) # Includes nodal variables

# Example 3: Keep zero-weight edges
decomposed_with_zeros <- decompose_netify(net_cs, remove_zeros = FALSE)

# Compare edge counts
nrow(decomposed_cs$edge_data) # Without zeros
nrow(decomposed_with_zeros$edge_data) # With zeros

# Example 4: Use for visualization prep
## Not run: 
##D # Decompose for use with ggplot2
##D plot_data <- decompose_netify(net_cs)
##D 
##D # Can now use edge_data and nodal_data separately
##D # for network visualization
## End(Not run)




cleanEx()
nameEx("ego_layouts")
### * ego_layouts

flush(stderr()); flush(stdout())

### Name: ego_layouts
### Title: Create ego-centric layouts for ego networks
### Aliases: ego_layouts create_hierarchical_ego_layout
###   create_radial_ego_layout create_ego_centric_layout

### ** Examples

## Not run: 
##D # Create ego network
##D ego_net <- ego_netify(my_network, ego = "Pakistan")
##D 
##D # Hierarchical layout
##D layout <- create_hierarchical_ego_layout(ego_net)
##D plot(ego_net, point_layout = layout)
##D 
##D # Radial layout with custom rings
##D layout <- create_radial_ego_layout(ego_net, n_rings = 5)
##D plot(ego_net, point_layout = layout)
## End(Not run)



cleanEx()
nameEx("gen_symm_id")
### * gen_symm_id

flush(stderr()); flush(stdout())

### Name: gen_symm_id
### Title: Generate symmetric identifiers for dyadic data
### Aliases: gen_symm_id

### ** Examples

# Create example dyadic data
trade_df <- data.frame(
    from = c("USA", "China", "Russia", "USA", "Brazil", "Argentina"),
    to = c("China", "USA", "USA", "Russia", "Argentina", "Brazil"),
    trade_value = c(100, 100, 50, 75, 30, 25),
    year = c(2020, 2020, 2021, 2021, 2021, 2021)
)

# Generate symmetric IDs without time
trade_df$symm_id <- gen_symm_id(trade_df, "from", "to")
print(trade_df[, c("from", "to", "symm_id")])
# Note: USA-China and China-USA both get "China_USA"

# Generate symmetric IDs with time
trade_df$symm_id_time <- gen_symm_id(trade_df, "from", "to", "year")
print(trade_df[, c("from", "to", "year", "symm_id_time")])
# Note: USA-China in 2020 gets "China_USA_2020"

# Use for aggregation of undirected relationships
trade_df$total_trade <- ave(
    trade_df$trade_value,
    trade_df$symm_id_time,
    FUN = sum
)
print(unique(trade_df[, c("symm_id_time", "total_trade")]))

# Example with longitudinal data
library(netify)
data(icews)
icews_sample <- icews[1:100, ]

# Create symmetric IDs for conflict events
icews_sample$symm_dyad <- gen_symm_id(
    icews_sample,
    actor1 = "i",
    actor2 = "j",
    time = "year"
)

# Check that symmetric pairs get same ID
icews_sample[icews_sample$i == "United States" & icews_sample$j == "Israel", "symm_dyad"]
icews_sample[icews_sample$i == "Israel" & icews_sample$j == "United States", "symm_dyad"]




cleanEx()
nameEx("get_adjacency")
### * get_adjacency

flush(stderr()); flush(stdout())

### Name: get_adjacency
### Title: Create a netify matrix from cross-sectional dyadic data
### Aliases: get_adjacency

### ** Examples

# Load example data
data(icews)

# Subset to one year for cross-sectional analysis
icews_2010 <- icews[icews$year == 2010, ]

# Create a directed network with verbal cooperation weights
verbCoop_net <- get_adjacency(
    dyad_data = icews_2010,
    actor1 = "i",
    actor2 = "j",
    symmetric = FALSE,
    weight = "verbCoop"
)

# Create a directed network with material conflict weights
matlConf_net <- get_adjacency(
    dyad_data = icews_2010,
    actor1 = "i",
    actor2 = "j",
    symmetric = FALSE,
    weight = "matlConf"
)

# Verify class
class(verbCoop_net) # "netify"

# Check dimensions
dim(verbCoop_net)




cleanEx()
nameEx("get_adjacency_array")
### * get_adjacency_array

flush(stderr()); flush(stdout())

### Name: get_adjacency_array
### Title: Create a netify array from longitudinal dyadic data
### Aliases: get_adjacency_array

### ** Examples

# Load example data
data(icews)

# Create a netify array (longitudinal directed network)
# with material conflict as edge weights
icews_array <- get_adjacency_array(
    dyad_data = icews,
    actor1 = "i",
    actor2 = "j",
    time = "year",
    symmetric = FALSE,
    weight = "matlConf"
)

# Verify it's a netify object
class(icews_array) # "netify"

# Check dimensions
dim(icews_array) # [n_actors, n_actors, n_years]

# Access specific time period
icews_2010 <- icews_array[, , "2010"]




cleanEx()
nameEx("get_adjacency_list")
### * get_adjacency_list

flush(stderr()); flush(stdout())

### Name: get_adjacency_list
### Title: Create a netify list from longitudinal dyadic data
### Aliases: get_adjacency_list

### ** Examples

# Load example data
data(icews)

# Create a netify list with constant actor composition
icews_list <- get_adjacency_list(
    dyad_data = icews,
    actor1 = "i",
    actor2 = "j",
    time = "year",
    actor_time_uniform = TRUE,
    symmetric = FALSE,
    weight = "verbConf"
)

# Verify it's a netify object
class(icews_list) # "netify"

# Check structure
length(icews_list) # Number of time periods
names(icews_list) # Time period labels

# Access specific time period
icews_2010 <- icews_list[["2010"]]
dim(icews_2010)




cleanEx()
nameEx("get_ego_layout")
### * get_ego_layout

flush(stderr()); flush(stdout())

### Name: get_ego_layout
### Title: Calculate ego-centric layout positions for network visualization
### Aliases: get_ego_layout

### ** Examples

## Not run: 
##D # Create an ego network
##D ego_net <- ego_netify(my_network, ego = "Alice")
##D 
##D # Get radial layout with alters grouped by attribute
##D layout_radial <- get_ego_layout(ego_net, 
##D                                layout = "radial",
##D                                group_by = "department")
##D 
##D # Get concentric layout with rings based on degree
##D layout_circles <- get_ego_layout(ego_net,
##D                                 layout = "concentric", 
##D                                 group_by = "degree_total")
##D 
##D # Use with plot
##D plot(ego_net, point_layout = layout_radial)
## End(Not run)




cleanEx()
nameEx("homophily")
### * homophily

flush(stderr()); flush(stdout())

### Name: homophily
### Title: Analyze homophily in network data
### Aliases: homophily

### ** Examples

## Not run: 
##D # Basic homophily analysis with default threshold (> 0)
##D homophily_default <- homophily(net, attribute = "group")
##D 
##D # Using different similarity metrics for continuous data
##D homophily_manhattan <- homophily(
##D     net,
##D     attribute = "age",
##D     method = "manhattan" # Less sensitive to outliers
##D )
##D 
##D # For binary attributes (e.g., gender, membership)
##D homophily_jaccard <- homophily(
##D     net,
##D     attribute = "member",
##D     method = "jaccard" # Better for binary data than correlation
##D )
##D 
##D # For categorical attributes
##D homophily_categorical <- homophily(
##D     net,
##D     attribute = "department",
##D     method = "categorical"
##D )
##D 
##D # Combining method and threshold
##D homophily_combined <- homophily(
##D     net,
##D     attribute = "score",
##D     method = "manhattan",
##D     threshold = function(x) quantile(x, 0.75, na.rm = TRUE)
##D )
## End(Not run)




cleanEx()
nameEx("icews")
### * icews

flush(stderr()); flush(stdout())

### Name: icews
### Title: Event data slice from ICEWS
### Aliases: icews
### Keywords: datasets

### ** Examples

data(icews)
icews[1:3, ]



cleanEx()
nameEx("layer_netify")
### * layer_netify

flush(stderr()); flush(stdout())

### Name: layer_netify
### Title: Create multilayer networks from multiple netify objects
### Aliases: layer_netify

### ** Examples

# Load example data
data(icews)

# Example 1: Cross-sectional multilayer network
icews_10 <- icews[icews$year == 2010, ]

# Create separate networks for different interaction types
verbal_coop <- netify(
    icews_10,
    actor1 = "i", actor2 = "j",
    symmetric = FALSE,
    weight = "verbCoop",
    nodal_vars = c("i_log_gdp", "i_log_pop"),
    dyad_vars = "verbConf"
)

material_coop <- netify(
    icews_10,
    actor1 = "i", actor2 = "j",
    symmetric = FALSE,
    weight = "matlCoop",
    nodal_vars = "i_polity2",
    dyad_vars = "matlConf"
)

# Layer them together
coop_multilayer <- layer_netify(
    netlet_list = list(verbal_coop, material_coop),
    layer_labels = c("Verbal", "Material")
)

# Check structure
dim(get_raw(coop_multilayer)) # [actors × actors × 2]
attr(coop_multilayer, "layers") # "Verbal" "Material"

# Example 2: Longitudinal multilayer (array format)
verbal_longit <- netify(
    icews,
    actor1 = "i", actor2 = "j", time = "year",
    symmetric = FALSE,
    weight = "verbCoop",
    output_format = "longit_array"
)

material_longit <- netify(
    icews,
    actor1 = "i", actor2 = "j", time = "year",
    symmetric = FALSE,
    weight = "matlCoop",
    output_format = "longit_array"
)

# Create longitudinal multilayer
longit_multilayer <- layer_netify(
    list(verbal_longit, material_longit),
    layer_labels = c("Verbal", "Material")
)

dim(get_raw(longit_multilayer)) # [actors × actors × 2 × years]




cleanEx()
nameEx("mexico")
### * mexico

flush(stderr()); flush(stdout())

### Name: mexico
### Title: Event data slice from UCDP on Mexico
### Aliases: mexico
### Keywords: datasets

### ** Examples

data(mexico)
mexico[1:3, ]



cleanEx()
nameEx("mutate_weights")
### * mutate_weights

flush(stderr()); flush(stdout())

### Name: mutate_weights
### Title: Mutate edge weights in a netify object
### Aliases: mutate_weights

### ** Examples

# Load example data
data(icews)
icews_2010 <- icews[icews$year == 2010, ]

# Create a weighted network
net <- netify(
    icews_2010,
    actor1 = "i", actor2 = "j",
    symmetric = FALSE,
    weight = "verbCoop"
)

# Example 1: Log transformation (common for skewed data)
net_log <- mutate_weights(
    net,
    transform_fn = log,
    add_constant = 1, # log(x + 1) to handle zeros
    new_name = "log_verbCoop"
)
print(net_log)

# Example 2: Square root transformation (moderate skewness)
net_sqrt <- mutate_weights(
    net,
    transform_fn = sqrt,
    new_name = "sqrt_verbCoop"
)

# Example 3: Binarization (convert to presence/absence)
net_binary <- mutate_weights(
    net,
    transform_fn = function(x) ifelse(x > 0, 1, 0),
    new_name = "verbCoop_binary"
)

# Example 4: Standardization (z-scores)
net_std <- mutate_weights(
    net,
    transform_fn = function(x) {
        mean_x <- mean(x, na.rm = TRUE)
        sd_x <- sd(x, na.rm = TRUE)
        return((x - mean_x) / sd_x)
    },
    new_name = "verbCoop_standardized"
)

# Example 5: Rank transformation
net_rank <- mutate_weights(
    net,
    transform_fn = function(x) rank(x, na.last = "keep"),
    new_name = "verbCoop_ranked"
)

# Example 6: Power transformation
net_power <- mutate_weights(
    net,
    transform_fn = function(x) x^0.5, # Square root as power
    new_name = "verbCoop_power"
)

# Example 7: Min-max normalization (scale to 0-1)
net_norm <- mutate_weights(
    net,
    transform_fn = function(x) {
        min_x <- min(x, na.rm = TRUE)
        max_x <- max(x, na.rm = TRUE)
        return((x - min_x) / (max_x - min_x))
    },
    new_name = "verbCoop_normalized"
)

# Example 8: Winsorization (cap extreme values)
net_winsor <- mutate_weights(
    net,
    transform_fn = function(x) {
        q95 <- quantile(x, 0.95, na.rm = TRUE)
        return(pmin(x, q95)) # Cap at 95th percentile
    },
    new_name = "verbCoop_winsorized"
)

# Example 9: Only add constant (no transformation function)
net_shifted <- mutate_weights(
    net,
    add_constant = 10,
    new_name = "verbCoop_shifted"
)

# Example 10: Don't keep original weights to save memory
net_log_compact <- mutate_weights(
    net,
    transform_fn = log1p, # log(1 + x), handles zeros automatically
    new_name = "log1p_verbCoop",
    keep_original = FALSE
)

# Example 11: Longitudinal network transformation
## Not run: 
##D # Create longitudinal network
##D net_longit <- netify(
##D     icews,
##D     actor1 = "i", actor2 = "j", time = "year",
##D     symmetric = FALSE,
##D     weight = "verbCoop",
##D     actor_time_uniform = FALSE
##D )
##D 
##D # Transform across all time periods
##D net_longit_log <- mutate_weights(
##D     net_longit,
##D     transform_fn = log1p,
##D     new_name = "log_verbCoop"
##D )
## End(Not run)

# Example 12: Custom transformation with multiple operations
net_custom <- mutate_weights(
    net,
    transform_fn = function(x) {
        # Complex transformation: log, then standardize
        x_log <- log(x + 1)
        x_std <- (x_log - mean(x_log, na.rm = TRUE)) / sd(x_log, na.rm = TRUE)
        return(x_std)
    },
    new_name = "verbCoop_log_std"
)




cleanEx()
nameEx("myanmar")
### * myanmar

flush(stderr()); flush(stdout())

### Name: myanmar
### Title: Event data slice from UCDP on Myanmar
### Aliases: myanmar
### Keywords: datasets

### ** Examples

data(myanmar)
myanmar[1:3, ]



cleanEx()
nameEx("netify")
### * netify

flush(stderr()); flush(stdout())

### Name: netify
### Title: Create network object from various data types
### Aliases: netify

### ** Examples


# load example directed event data from ICEWS
# this data comes in the form of a dyadic
# dataframe where all dyad pairs are listed
data(icews)

# From a data.frame: generate a longitudional, directed and weighted network
# where the weights are matlConf
icews_matlConf <- netify(
    input = icews,
    actor1 = "i", actor2 = "j", time = "year",
    symmetric = FALSE, weight = "matlConf"
)

# From a matrix
adj_matrix <- matrix(rbinom(100, 1, 0.3), 10, 10)
net_from_matrix <- netify(adj_matrix)

# From an igraph object
## Not run: 
##D library(igraph)
##D g <- sample_gnp(10, 0.3)
##D net_from_igraph <- netify(g)
## End(Not run)




cleanEx()
nameEx("netify_edge")
### * netify_edge

flush(stderr()); flush(stdout())

### Name: netify_edge
### Title: Extract edges layer from netify plot components
### Aliases: netify_edge

### ** Examples

## Not run: 
##D # create a netify object
##D net <- netify(my_data, actor1 = "from", actor2 = "to")
##D 
##D # get plot components
##D comp <- plot(net, return_components = TRUE)
##D 
##D # build custom plot with edges
##D library(ggplot2)
##D ggplot() +
##D     netify_edge(comp)
## End(Not run)




cleanEx()
nameEx("netify_label")
### * netify_label

flush(stderr()); flush(stdout())

### Name: netify_label
### Title: Extract label layer from netify plot components
### Aliases: netify_label

### ** Examples

## Not run: 
##D # create a netify object
##D net <- netify(my_data, actor1 = "from", actor2 = "to")
##D 
##D # get plot components with labels
##D comp <- plot(net, add_label = TRUE, return_components = TRUE)
##D 
##D # build custom plot with labels
##D library(ggplot2)
##D ggplot() +
##D     netify_label(comp)
## End(Not run)




cleanEx()
nameEx("netify_label_repel")
### * netify_label_repel

flush(stderr()); flush(stdout())

### Name: netify_label_repel
### Title: Extract label_repel layer from netify plot components
### Aliases: netify_label_repel

### ** Examples

## Not run: 
##D # create a netify object
##D net <- netify(my_data, actor1 = "from", actor2 = "to")
##D 
##D # get plot components with label_repel
##D comp <- plot(net, add_label_repel = TRUE, return_components = TRUE)
##D 
##D # build custom plot with repelled labels
##D library(ggplot2)
##D ggplot() +
##D     netify_label_repel(comp)
## End(Not run)




cleanEx()
nameEx("netify_node")
### * netify_node

flush(stderr()); flush(stdout())

### Name: netify_node
### Title: Extract nodes layer from netify plot components
### Aliases: netify_node

### ** Examples

## Not run: 
##D # create a netify object
##D net <- netify(my_data, actor1 = "from", actor2 = "to")
##D 
##D # get plot components
##D comp <- plot(net, return_components = TRUE)
##D 
##D # build custom plot with nodes
##D library(ggplot2)
##D ggplot() +
##D     netify_node(comp)
## End(Not run)




cleanEx()
nameEx("netify_scale_labels")
### * netify_scale_labels

flush(stderr()); flush(stdout())

### Name: netify_scale_labels
### Title: Set scale labels for netify plots
### Aliases: netify_scale_labels

### ** Examples

## Not run: 
##D # set labels for different scales
##D plot(my_netify_obj,
##D     edge_alpha_var = "weight",
##D     point_size_var = "degree"
##D ) +
##D     netify_scale_labels(
##D         edge_alpha = "Connection Strength",
##D         node_size = "Node Degree" # node_* is converted to point_*
##D     )
## End(Not run)




cleanEx()
nameEx("netify_text")
### * netify_text

flush(stderr()); flush(stdout())

### Name: netify_text
### Title: Extract text layer from netify plot components
### Aliases: netify_text

### ** Examples

## Not run: 
##D # create a netify object
##D net <- netify(my_data, actor1 = "from", actor2 = "to")
##D 
##D # get plot components with text labels
##D comp <- plot(net, add_text = TRUE, return_components = TRUE)
##D 
##D # build custom plot with text
##D library(ggplot2)
##D ggplot() +
##D     netify_text(comp)
## End(Not run)




cleanEx()
nameEx("netify_text_repel")
### * netify_text_repel

flush(stderr()); flush(stdout())

### Name: netify_text_repel
### Title: Extract text_repel layer from netify plot components
### Aliases: netify_text_repel

### ** Examples

## Not run: 
##D # create a netify object
##D net <- netify(my_data, actor1 = "from", actor2 = "to")
##D 
##D # get plot components with text_repel
##D comp <- plot(net, add_text_repel = TRUE, return_components = TRUE)
##D 
##D # build custom plot with repelled text
##D library(ggplot2)
##D ggplot() +
##D     netify_text_repel(comp)
## End(Not run)




cleanEx()
nameEx("netify_to_amen")
### * netify_to_amen

flush(stderr()); flush(stdout())

### Name: netify_to_amen
### Title: Convert netify objects to amen format
### Aliases: netify_to_amen to_amen

### ** Examples

# Load example data
data(icews)

# Create a netify object
net <- netify(
    icews[icews$year == 2010, ],
    actor1 = "i", actor2 = "j",
    symmetric = FALSE,
    weight = "verbCoop"
)

# Convert to amen format (standard)
amen_data <- netify_to_amen(net)
names(amen_data) # Y, Xdyad, Xrow, Xcol

## Not run: 
##D # For longitudinal data with time-varying composition
##D longit_net <- netify(
##D     icews,
##D     actor1 = "i", actor2 = "j", time = "year",
##D     symmetric = FALSE,
##D     weight = "verbCoop"
##D )
##D 
##D # Convert to lame format
##D lame_data <- netify_to_amen(longit_net, lame = TRUE)
## End(Not run)




cleanEx()
nameEx("netify_to_igraph")
### * netify_to_igraph

flush(stderr()); flush(stdout())

### Name: netify_to_igraph
### Title: Convert netify objects to igraph format
### Aliases: netify_to_igraph to_igraph

### ** Examples

# Load example data
data(icews)

# Example 1: Cross-sectional network with attributes
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

# Convert to igraph
ig <- netify_to_igraph(verbCoop_net)

# Examine the result
ig
igraph::vcount(ig) # number of vertices
igraph::ecount(ig) # number of edges
igraph::vertex_attr_names(ig) # nodal attributes
igraph::edge_attr_names(ig) # edge attributes

# Access specific attributes
igraph::V(ig)$i_polity2 # polity scores
igraph::E(ig)$matlCoop # material cooperation

# Example 2: Longitudinal network
verbCoop_longit <- netify(
    icews,
    actor1 = "i", actor2 = "j", time = "year",
    symmetric = FALSE,
    weight = "verbCoop",
    dyad_vars = dvars,
    dyad_vars_symmetric = rep(FALSE, length(dvars)),
    nodal_vars = nvars
)

# Convert to list of igraph objects
ig_list <- netify_to_igraph(verbCoop_longit)

# Examine structure
length(ig_list) # number of time periods
names(ig_list) # time period labels

# Access specific time period
ig_2002 <- ig_list[["2002"]]
ig_2002

# Example 3: Convert without attributes
ig_structure_only <- netify_to_igraph(
    verbCoop_net,
    add_nodal_attribs = FALSE,
    add_dyad_attribs = FALSE
)

# Only network structure, no attributes
igraph::vertex_attr_names(ig_structure_only) # only "name"
igraph::edge_attr_names(ig_structure_only) # only "weight" (if present)




cleanEx()
nameEx("netify_to_statnet")
### * netify_to_statnet

flush(stderr()); flush(stdout())

### Name: netify_to_statnet
### Title: Convert netify objects to statnet network format
### Aliases: netify_to_statnet netify_to_network to_statnet to_network

### ** Examples

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
network::network.size(ntwk) # number of vertices
network::network.edgecount(ntwk) # number of edges
network::list.vertex.attributes(ntwk) # nodal attributes
network::list.edge.attributes(ntwk) # edge attributes

# Access specific attributes
network::get.vertex.attribute(ntwk, "i_polity2") # polity scores
network::get.edge.attribute(ntwk, "matlCoop") # material cooperation

# Check network properties
network::is.directed(ntwk) # TRUE for this example
network::has.loops(ntwk) # FALSE (no self-ties)

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
names(ntwk_list) # time period labels

# Access specific time period
ntwk_2002 <- ntwk_list[["2002"]]
ntwk_2002

## Not run: 
##D # Use with ergm for modeling (requires ergm package)
##D library(ergm)
##D model <- ergm(ntwk ~ edges + mutual + nodematch("i_polity2"))
## End(Not run)




cleanEx()
nameEx("peek")
### * peek

flush(stderr()); flush(stdout())

### Name: peek
### Title: Preview subsets of network data from netify objects
### Aliases: peek

### ** Examples

# Load example data
data(icews)

# Example 1: Basic usage with cross-sectional network
icews_10 <- icews[icews$year == 2010, ]
net <- netify(
    icews_10,
    actor1 = "i", actor2 = "j",
    weight = "verbCoop"
)

# Default: see first 3 actors (both sending and receiving)
peek(net)

# See first 5 senders and first 5 receivers
peek(net, from = 5, to = 5)

# Example 2: Specific actors by name
# See ties from US and China to Russia, India, and Brazil
peek(net,
    from = c("United States", "China"),
    to = c("Russia", "India", "Brazil")
)

# Use actors parameter to see subgraph
peek(net,
    actors = c("United States", "China", "Russia")
)

# Example 3: Longitudinal network
net_longit <- netify(
    icews,
    actor1 = "i", actor2 = "j", time = "year",
    weight = "matlConf"
)

# See first 5 actors in specific years
peek(net_longit,
    from = 5, to = 5,
    time = c("2002", "2006", "2010")
)

# See all actors in year 2010
peek(net_longit,
    from = NULL, to = NULL,
    time = "2010"
)

# Example 4: Using numeric indices
# See specific positions in the network
peek(net,
    from = c(1, 3, 5, 7), # 1st, 3rd, 5th, 7th senders
    to = 1:10
) # first 10 receivers

# Example 5: Quick inspection patterns
# See who USA interacts with
peek(net, from = "United States", to = 10) # USA's ties to first 10 countries
peek(net, from = 10, to = "United States") # First 10 countries' ties to USA




cleanEx()
nameEx("pivot_dyad_to_network")
### * pivot_dyad_to_network

flush(stderr()); flush(stdout())

### Name: pivot_dyad_to_network
### Title: Pivot a dyadic variable to become the network
### Aliases: pivot_dyad_to_network

### ** Examples

# Load example data
data(icews)

# Create a netify object with verbal cooperation as the main network
# and material cooperation as a dyadic attribute
icews_10 <- icews[icews$year == 2010, ]

net <- netify(
    icews_10,
    actor1 = "i", actor2 = "j",
    symmetric = FALSE,
    weight = "verbCoop"
)

net <- add_dyad_vars(
    net,
    icews_10,
    actor1 = "i", actor2 = "j",
    dyad_vars = "matlCoop",
    dyad_vars_symmetric = FALSE
)

# Check the current network
print(net)

# Pivot to make material cooperation the main network
net_pivoted <- pivot_dyad_to_network(
    net,
    dyad_var = "matlCoop",
    network_var_name = "verbCoop"
)

# The main network is now material cooperation
print(net_pivoted)

# The old network (verbal cooperation) is preserved as a dyadic attribute
attr(net_pivoted, "dyad_data")[["1"]][["verbCoop"]][1:5, 1:5]




cleanEx()
nameEx("plot.netify")
### * plot.netify

flush(stderr()); flush(stdout())

### Name: plot.netify
### Title: Plotting method for netify objects
### Aliases: plot.netify

### ** Examples

# Load example data
data(icews)

# Basic cross-sectional network
icews_10 <- icews[icews$year == 2010, ]
net_10 <- netify(
    icews_10,
    actor1 = "i", actor2 = "j",
    symmetric = FALSE,
    weight = "verbCoop"
)

# Simple plot
plot(net_10)

# add nodal stats to netlet
net_10 <- add_node_vars(
    net_10,
    summary_actor(net_10),
    "actor"
)

# Customized plot with new naming convention
plot(net_10,
    edge_color = "lightgrey",
    node_size_by = "degree_total", # Instead of point_size_var
    node_color = "steelblue",
    edge_alpha_by = "verbCoop", # Instead of edge_alpha_var
    node_size_label = "Degree",
    edge_alpha_label = "Verbal Cooperation"
)

# Longitudinal network example
net_longit <- netify(
    icews,
    actor1 = "i", actor2 = "j",
    time = "year",
    symmetric = FALSE,
    weight = "verbCoop",
    nodal_vars = c("i_polity2", "i_log_gdp")
)

# Add network statistics
net_longit <- add_node_vars(
    net_longit,
    summary_actor(net_longit),
    actor = "actor",
    time = "time"
)

# Plot with multiple aesthetics
plot(net_longit,
    # Edges
    edge_color = "grey70",
    mutate_weight = log1p, # Transform weights
    # Nodes
    node_size_by = "degree_total",
    node_color_by = "i_polity2",
    # Labels
    node_size_label = "Total Degree",
    node_color_label = "Polity Score",
    edge_alpha_label = "Log(Verbal Coop.)",
    # Layout
    static_actor_positions = TRUE # Keep positions constant
)

# Selective labeling example
plot(net_10,
    node_size_by = "degree_total",
    select_text = c("United States", "China", "Russian Federation"),
    text_size = 3,
    text_color = "darkred"
)

# choose alternative labels for selected text
plot(net_10,
    node_size_by = "degree_total",
    select_text = c("United States", "China", "Russian Federation"),
    select_text_display = c("USA", "CHN", "RUS"),
    text_size = 3,
    text_color = "darkred"
)


# Time subsetting example
plot(net_longit,
    time = c("2010", "2011", "2012")
)

# Node subsetting example
# democracies with high GDP
plot(net_longit, node_filter = ~ i_polity2 > 6 & i_log_gdp > 25)

# use return_components=TRUE
# to get back ggplot2 pieces of plot
g10 <- plot(
    net_10,
    node_alpha = .8,
    arrow = ggplot2::arrow(length = ggplot2::unit(0.01, "inches")),
    node_size_by = "degree_total",
    node_size_label = "Log(Degree)",
    edge_alpha_label = "Log(Verbal Coop.)",
    remove_isolates = TRUE,
    mutate_weight = log1p,
    return_components = TRUE
)

# Manually assemble with custom modifications
# to scale aesthetics such as edges
g10$base +
    netify_edge(g10) +
    ggplot2::scale_alpha_continuous(range = c(0.01, 0.2)) +
    netify_node(g10) +
    theme_netify()




cleanEx()
nameEx("plot_homophily")
### * plot_homophily

flush(stderr()); flush(stdout())

### Name: plot_homophily
### Title: Visualize homophily analysis results
### Aliases: plot_homophily

### ** Examples

## Not run: 
##D # Load example data
##D data(icews)
##D 
##D # Create a simple network
##D ntwk <- netify(
##D     icews,
##D     actor1 = "i", actor2 = "j",
##D     time = "year",
##D     symmetric = FALSE,
##D     weight = "matlCoop"
##D )
##D 
##D # Run homophily analysis
##D homophily_result <- homophily(
##D     ntwk,
##D     attribute = "i_polity2",
##D     method = "correlation"
##D )
##D 
##D # Create distribution plot
##D plot_homophily(
##D     homophily_result,
##D     netlet = ntwk,
##D     type = "distribution",
##D     attribute = "i_polity2"
##D )
## End(Not run)




cleanEx()
nameEx("plot_mixing_matrix")
### * plot_mixing_matrix

flush(stderr()); flush(stdout())

### Name: plot_mixing_matrix
### Title: Visualize attribute mixing matrix results
### Aliases: plot_mixing_matrix

### ** Examples

## Not run: 
##D # Create a network with categorical attributes
##D data(icews)
##D icews_10 <- icews[icews$year == 2010, ]
##D net <- netify(
##D     icews_10,
##D     actor1 = "i", actor2 = "j",
##D     symmetric = FALSE,
##D     weight = "verbCoop"
##D )
##D 
##D # Run mixing matrix analysis
##D mixing_result <- mixing_matrix(
##D     net,
##D     attribute = "i_polity2_cat"
##D )
##D 
##D # Create visualization
##D plot_mixing_matrix(mixing_result)
## End(Not run)




cleanEx()
nameEx("plot_mixing_matrix_facet")
### * plot_mixing_matrix_facet

flush(stderr()); flush(stdout())

### Name: plot_mixing_matrix_facet
### Title: Create a multi-panel mixing matrix visualization
### Aliases: plot_mixing_matrix_facet

### ** Examples

## Not run: 
##D # Create temporal network
##D data(icews)
##D net_temporal <- netify(
##D     icews,
##D     actor1 = "i", actor2 = "j",
##D     time = "year",
##D     symmetric = FALSE,
##D     weight = "verbCoop"
##D )
##D 
##D # Run mixing matrix analysis across time
##D mixing_temporal <- mixing_matrix(
##D     net_temporal,
##D     attribute = "i_polity2_cat"
##D )
##D 
##D # Create faceted visualization
##D plot_mixing_matrix_facet(mixing_temporal, ncol = 2)
## End(Not run)




cleanEx()
nameEx("print.netify_plot_components")
### * print.netify_plot_components

flush(stderr()); flush(stdout())

### Name: print.netify_plot_components
### Title: Print netify plot components
### Aliases: print.netify_plot_components

### ** Examples

## Not run: 
##D # create plot components
##D comp <- plot(my_netify_obj, return_components = TRUE)
##D 
##D # print summary
##D print(comp)
## End(Not run)




cleanEx()
nameEx("remove_ego_edges")
### * remove_ego_edges

flush(stderr()); flush(stdout())

### Name: remove_ego_edges
### Title: Remove ego-alter edges from ego network
### Aliases: remove_ego_edges

### ** Examples

## Not run: 
##D ego_net <- ego_netify(my_network, ego = "Alice")
##D alter_only_net <- remove_ego_edges(ego_net)
##D plot(alter_only_net)
## End(Not run)



cleanEx()
nameEx("reset_scales")
### * reset_scales

flush(stderr()); flush(stdout())

### Name: reset_scales
### Title: Reset aesthetic scales in ggplot
### Aliases: reset_scales

### ** Examples

## Not run: 
##D # create a plot with different colors for edges and nodes
##D comp <- plot(net, return_components = TRUE)
##D 
##D ggplot() +
##D     netify_edge(comp) +
##D     scale_color_manual(values = c("gray", "red")) +
##D     reset_scales() + # reset before adding nodes
##D     netify_node(comp) +
##D     scale_color_viridis_c()
## End(Not run)




cleanEx()
nameEx("subset.netify")
### * subset.netify

flush(stderr()); flush(stdout())

### Name: subset.netify
### Title: Subset netify objects
### Aliases: subset.netify

### ** Examples


# load example directed event data from ICEWS
data(icews)

# generate a longitudional netify object
# with both dyadic and nodal attributes
icews_matlConf <- netify(
    icews,
    actor1 = "i", actor2 = "j", time = "year",
    symmetric = FALSE, weight = "matlConf",
    nodal_vars = c("i_polity2", "i_log_gdp", "i_log_pop"),
    dyad_vars = c("matlCoop", "verbCoop", "verbConf"),
    dyad_vars_symmetric = c(FALSE, FALSE, FALSE)
)

# subset to a few countries using S3 method
icews_subset <- subset(
    icews_matlConf,
    actors = c(
        "United States", "United Kingdom",
        "Russian Federation", "China"
    )
)

# subset to a few countries and a few years
icews_subset_2 <- subset(
    icews_matlConf,
    actors = c(
        "United States", "United Kingdom",
        "Russian Federation", "China"
    ),
    time = c("2010", "2011")
)

# can also use subset_netify directly
icews_subset_3 <- subset_netify(
    netlet = icews_matlConf,
    actors = c(
        "United States", "United Kingdom",
        "Russian Federation", "China"
    ),
    time = c("2010", "2011")
)




cleanEx()
nameEx("summary.netify")
### * summary.netify

flush(stderr()); flush(stdout())

### Name: summary.netify
### Title: Calculate graph-level statistics for netify objects
### Aliases: summary.netify

### ** Examples

# Load example data
data(icews)

# Basic usage
net <- netify(
    icews,
    actor1 = "i", actor2 = "j", time = "year",
    symmetric = FALSE,
    weight = "verbCoop"
)

# get summary
summary(net)

## Not run: 
##D # Add custom statistics - community detection
##D comm_stats <- function(mat) {
##D     g <- netify_to_igraph(mat)
##D     comm <- igraph::cluster_spinglass(g)
##D     c(
##D         n_communities = length(comm$csize),
##D         modularity = comm$modularity
##D     )
##D }
##D 
##D # Apply to subset for efficiency
##D sub_net <- subset_netify(net, time = as.character(2013:2014))
##D summary(sub_net, other_stats = list(community = comm_stats))
## End(Not run)




cleanEx()
nameEx("summary_actor")
### * summary_actor

flush(stderr()); flush(stdout())

### Name: summary_actor
### Title: Calculate actor-level network statistics
### Aliases: summary_actor

### ** Examples

# Load example data
data(icews)

# Basic usage with directed network
net <- netify(
    icews,
    actor1 = "i", actor2 = "j", time = "year",
    symmetric = FALSE,
    weight = "verbCoop"
)

# Get actor statistics
actor_stats <- summary_actor(net)
head(actor_stats)

# Add custom statistics
# Maximum incoming and outgoing tie weights
max_out <- function(mat) apply(mat, 1, max, na.rm = TRUE)
max_in <- function(mat) apply(mat, 2, max, na.rm = TRUE)

actor_stats_custom <- summary_actor(
    net,
    other_stats = list(
        max_out = max_out,
        max_in = max_in
    )
)
head(actor_stats_custom)

# For networks where weights represent distances
# (larger values = weaker relationships)
distance_net <- netify(
    icews,
    actor1 = "i", actor2 = "j",
    weight = "matlConf" # conflict measure
)

# Don't invert weights for centrality calculations
actor_stats_dist <- summary_actor(
    distance_net,
    invert_weights_for_igraph = FALSE
)




cleanEx()
nameEx("to_netify")
### * to_netify

flush(stderr()); flush(stdout())

### Name: to_netify
### Title: Convert igraph, network, or matrix objects to netify format
### Aliases: to_netify

### ** Examples

## Not run: 
##D # From igraph
##D library(igraph)
##D g <- sample_gnp(10, 0.3)
##D E(g)$weight <- runif(ecount(g))
##D V(g)$type <- sample(c("A", "B"), vcount(g), replace = TRUE)
##D 
##D net <- to_netify(g, weight = "weight")
##D 
##D # From network
##D library(network)
##D n <- network(rgraph(10, tprob = 0.3))
##D set.vertex.attribute(n, "group", sample(1:2, 10, replace = TRUE))
##D 
##D net <- to_netify(n)
##D 
##D # From matrix
##D adj_mat <- matrix(rnorm(100), 10, 10)
##D net <- to_netify(adj_mat)
##D 
##D # From list of matrices (longitudinal)
##D mat_list <- list(
##D     "2001" = matrix(rnorm(100), 10, 10),
##D     "2002" = matrix(rnorm(100), 10, 10)
##D )
##D net <- to_netify(mat_list)
## End(Not run)




cleanEx()
nameEx("unnetify")
### * unnetify

flush(stderr()); flush(stdout())

### Name: unnetify
### Title: Convert netify objects back to dyadic data frames
### Aliases: unnetify netify_to_df

### ** Examples

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
names(dyad_df)

# Remove zero-weight dyads for more compact output
dyad_df_nonzero <- unnetify(verbCoop_net, remove_zeros = TRUE)
nrow(dyad_df_nonzero) # Much smaller than full dyadic dataset

# Note how nodal attributes are added
# For directed network: _from and _to suffixes
head(dyad_df[, c("from", "to", "i_polity2_from", "i_polity2_to")])

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

# Each dyad now has associated time period
# Note: weight column is named after the weight variable (verbCoop)
head(dyad_df_longit[, c("from", "to", "time", "verbCoop")])

# Use the output for further analysis
## Not run: 
##D # For example, regression analysis
##D lm(verbCoop ~ i_polity2_from + i_polity2_to + verbConf, data = dyad_df)
## End(Not run)




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
