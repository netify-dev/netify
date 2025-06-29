% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/decompose_rs.R
\name{decompose_netify}
\alias{decompose_netify}
\title{Decompose a netify object into edge and node data frames}
\usage{
decompose_netify(netlet, remove_zeros = TRUE)
}
\arguments{
\item{netlet}{A netify object (class "netify") to be decomposed.}

\item{remove_zeros}{Logical. If TRUE (default), edges with zero weight values
are removed from the edge data frame. If FALSE, zero-weight edges are retained.}
}
\value{
A list containing two data frames:
\itemize{
\item \strong{edge_data}: A data frame where each row represents an edge with columns:
\itemize{
\item \code{from}: Source node identifier
\item \code{to}: Target node identifier
\item \code{time}: Time period (character; "1" for cross-sectional networks)
\item \code{weight}: Edge weight values (using original weight variable name if specified)
\item Additional columns for any dyadic variables stored in the netify object
}
\item \strong{nodal_data}: A data frame where each row represents a node-time combination with columns:
\itemize{
\item \code{name}: Node identifier
\item \code{time}: Time period (character; "1" for cross-sectional networks)
\item Additional columns for any nodal variables stored in the netify object
}
}
}
\description{
\code{decompose_netify} separates a netify
object into its constituent parts: a data frame of edges with attributes and
a data frame of nodal attributes.
}
\details{
The function helpful for:

\strong{Edge data processing:}
\itemize{
\item Extracts the adjacency matrix (or array for longitudinal networks) from the netify object
\item Optionally removes zero-weight edges based on the remove_zeros parameter
\item Merges any dyadic variables stored in the netify object
\item Renames columns to standardized names (from, to, time)
}

\strong{Node data processing:}
\itemize{
\item Extracts nodal attributes if present, or constructs from actor_pds information
\item Ensures consistent time variable across node and edge data
\item Renames columns to standardized names (name, time)
}

\strong{Time handling:}
\itemize{
\item For cross-sectional networks: Sets time to "1" in both data frames
\item For longitudinal networks: Preserves original time periods as character values
\item For ego networks: Extracts time from ego-time concatenated identifiers
}

\strong{Variable preservation:}

All dyadic and nodal variables stored in the netify object are preserved in the
output data frames. Dyadic variables are merged with the edge data, while nodal
variables remain in the nodal data frame.
}
\examples{
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
\dontrun{
# Decompose for use with ggplot2
plot_data <- decompose_netify(net_cs)

# Can now use edge_data and nodal_data separately
# for network visualization
}

}
\author{
Cassy Dorff, Shahryar Minhas
}
