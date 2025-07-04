% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/to_statnet.R
\name{netify_to_statnet}
\alias{netify_to_statnet}
\alias{netify_to_network}
\alias{to_statnet}
\alias{to_network}
\title{Convert netify objects to statnet network format}
\usage{
netify_to_statnet(netlet, add_nodal_attribs = TRUE, add_dyad_attribs = TRUE)

netify_to_network(netlet, add_nodal_attribs = TRUE, add_dyad_attribs = TRUE)

to_statnet(netlet, add_nodal_attribs = TRUE, add_dyad_attribs = TRUE)

to_network(netlet, add_nodal_attribs = TRUE, add_dyad_attribs = TRUE)
}
\arguments{
\item{netlet}{A netify object containing network data. Currently supports
single-layer networks only. For multilayer networks, use
\code{\link{subset_netify}} to extract individual layers first.}

\item{add_nodal_attribs}{Logical. If TRUE (default), includes nodal attributes
from the netify object as vertex attributes in the network object. Set to
FALSE to create a network with structure only.}

\item{add_dyad_attribs}{Logical. If TRUE (default), includes dyadic attributes
from the netify object as edge attributes in the network object. Set to
FALSE to exclude edge covariates.}
}
\value{
A network object or list of network objects:
\describe{
\item{Cross-sectional networks}{Returns a single network object}
\item{Longitudinal networks}{Returns a named list of network objects,
with names corresponding to time periods}
}

The resulting network object(s) will have:
\itemize{
\item Vertices named according to actors in the netify object
\item Edge weights from the netify weight variable stored as "weight" attribute
\item Vertex attributes for each nodal variable (if add_nodal_attribs = TRUE)
\item Edge attributes for each dyadic variable (if add_dyad_attribs = TRUE)
\item Proper directedness based on the symmetric parameter of the netify object
}
}
\description{
Transforms netify network objects into statnet's network
objects (also available as \code{netify_to_network},
\code{to_statnet}, and \code{to_network}),
providing access to the extensive statistical modeling capabilities
of the statnet suite, including ERGMs (Exponential Random Graph Models),
descriptive statistics, and network visualization tools.
}
\details{
The conversion process handles different netify structures:
\itemize{
\item \strong{Cross-sectional}: Direct conversion to a single network object
\item \strong{Longitudinal arrays}: Internally converted to list format, then
each time slice becomes a separate network object
\item \strong{Longitudinal lists}: Each time period converted to separate network object
}

The statnet network format stores networks as an edgelist with attributes,
making it memory-efficient for sparse networks. All nodal and dyadic attributes
from the netify object are preserved and can be used in subsequent ERGM
modeling or network analysis.

For longitudinal data, each time period results in an independent network
object. This format is suitable for discrete-time network analysis or
pooled ERGM estimation across time periods.
}
\note{
This function requires the network package (part of statnet) to be installed.

For ERGM modeling, the ergm package (also part of statnet) should be loaded
after creating the network objects.
}
\examples{
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

\dontrun{
# Use with ergm for modeling (requires ergm package)
library(ergm)
model <- ergm(ntwk ~ edges + mutual + nodematch("i_polity2"))
}

}
\author{
Ha Eun Choi, Cassy Dorff, Colin Henry, Shahryar Minhas
}
