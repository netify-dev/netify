% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_dyad_vars.R
\name{add_dyad_vars}
\alias{add_dyad_vars}
\alias{add_edge_attributes}
\title{Add dyadic variables to a netify object}
\usage{
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
}
\arguments{
\item{netlet}{A netify object (class "netify") to which dyadic variables will be added.}

\item{dyad_data}{A data.frame object containing the dyadic variables to add. Must
include columns matching the actor1, actor2, and time specifications used
in the original netify object. Will be coerced to data.frame if a tibble or
data.table is provided.}

\item{actor1}{Character string specifying the column name in dyad_data for the first actor
in each dyad. Should match the actor1 specification used when creating the
netify object.}

\item{actor2}{Character string specifying the column name in dyad_data for the second actor
in each dyad. Should match the actor2 specification used when creating the
netify object.}

\item{time}{Character string specifying the column name in dyad_data for time periods.
Required for longitudinal netify objects. Should match the time specification
used when creating the netify object. Set to NULL for cross-sectional networks.}

\item{dyad_vars}{Character vector of column names from dyad_data to add as
dyadic variables. If NULL (default), all columns except actor1, actor2,
and time will be added.}

\item{dyad_vars_symmetric}{Logical vector indicating whether each dyadic
variable represents symmetric relationships. Must have the same length as
dyad_vars. If NULL, defaults to the symmetry setting of the netify object,
but a warning will be issued recommending explicit specification.}

\item{replace_existing}{Logical scalar. If TRUE, existing dyadic variables with the
same names will be replaced. If FALSE (default), attempting to add variables
that already exist will result in an error.}
}
\value{
A netify object (class "netify") with the additional dyadic variables stored in the
'dyad_data' attribute. The structure is a nested list where:
\itemize{
\item First level: named list with time periods as names (or "1" for cross-sectional data)
\item Second level: named list with variable names as names
\item Values: matrix objects with actors as rows/columns and numeric, integer,
logical, or character values
}
}
\description{
\code{add_dyad_vars} (also available as \code{add_edge_attributes})
merges additional dyadic (edge-level) variables from a data.frame
into an existing netify object. This function allows you to incrementally
build up the dyadic attributes of your network after initial creation, which
is useful when variables come from different sources or need different
preprocessing.
}
\details{
Dyadic variables are stored as matrix objects where rows represent the first actor
(sender in directed networks) and columns represent the second actor (receiver
in directed networks). For symmetric variables in undirected networks, the
function ensures that \code{matrix[i,j]} equals \code{matrix[j,i]}.

The function optimizes storage by automatically detecting the data type of
each variable and using the appropriate matrix storage mode:
\itemize{
\item logical vectors → logical matrices
\item integer vectors → integer matrices
\item numeric vectors with only integer values → integer matrices
\item numeric vectors with decimals → double matrices
\item character vectors → character matrices
}

For longitudinal networks, the function handles time-varying actor sets
appropriately, creating matrices that include only actors present at each
time point.

Missing dyadic observations (NA values) in the input data.frame will be set to missing
in the resulting matrices as well.
}
\note{
The input \code{dyad_data} must be a \code{data.frame} or an object that can be coerced into a \code{data.frame}
(e.g., a \code{tibble} or \code{data.table}). Inputs such as matrices or arrays are not supported.

When adding dyadic variables to bipartite networks, all variables are
automatically treated as asymmetric regardless of the dyad_vars_symmetric
specification.

For large networks, consider the memory implications of adding many dyadic
variables, as each variable requires a full adjacency matrix for storage.
}
\examples{
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
\dontrun{
# Create a new data.frame with trade information
trade_data <- data.frame(
    i = icews_10$i,
    j = icews_10$j,
    trade_volume = runif(nrow(icews_10), 0, 1000),
    trade_balance = rnorm(nrow(icews_10))
)
class(trade_data) # "data.frame"

verbCoop_net <- add_dyad_vars(
    netlet = verbCoop_net,
    dyad_data = trade_data,
    actor1 = "i", actor2 = "j",
    dyad_vars = c("trade_volume", "trade_balance"),
    dyad_vars_symmetric = c(FALSE, FALSE)
)
}

}
\author{
Cassy Dorff, Colin Henry, Shahryar Minhas
}
