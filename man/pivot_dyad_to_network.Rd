% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pivot_dyad_to_network.R
\name{pivot_dyad_to_network}
\alias{pivot_dyad_to_network}
\title{Pivot a dyadic variable to become the network}
\usage{
pivot_dyad_to_network(
  netlet,
  dyad_var,
  make_network_dyad_var = TRUE,
  network_var_name = NULL,
  symmetric = NULL,
  weight_type = NULL,
  diag_to_NA = NULL,
  missing_to_zero = NULL
)
}
\arguments{
\item{netlet}{A netify object containing the network and dyadic attributes}

\item{dyad_var}{Character string naming the dyadic variable to become the new network.
Must exist in the netlet's dyad_data attribute.}

\item{make_network_dyad_var}{Logical. If TRUE (default), the current network
will be preserved as a dyadic attribute.}

\item{network_var_name}{Character string specifying the name for the old network
when converted to a dyadic attribute. If NULL (default), uses the current
weight attribute name if available, otherwise "old_network".}

\item{symmetric}{Logical or NULL. Specifies whether the new network should be
treated as symmetric. If NULL (default), attempts to detect from the dyadic
variable's symmetry setting or data structure.}

\item{weight_type}{Character string describing the type of weight for the new
network (e.g., "trade_volume", "fdi_amount"). If NULL (default), uses the
dyad_var name.}

\item{diag_to_NA}{Logical. Whether to set diagonal values to NA in the new
network. If NULL (default), inherits from the original netlet.}

\item{missing_to_zero}{Logical. Whether to treat missing values as zeros in
the new network. If NULL (default), inherits from the original netlet.}
}
\value{
A netify object with the dyadic variable as the main network and
(optionally) the old network preserved as a dyadic attribute. All other
attributes and dyadic variables are preserved.
}
\description{
\code{pivot_dyad_to_network} swaps a dyadic attribute with the main network in a netify object.
This is useful when you want to analyze a different relationship type that was stored
as a dyadic attribute. For example, if your network represents trade relationships
but you have FDI stored as a dyadic attribute, you can pivot to make FDI the main network.
}
\details{
The function handles different netify types appropriately:
\itemize{
\item For cross-sectional networks: performs a simple matrix swap
\item For longitudinal arrays: swaps matrices across all time periods
\item For longitudinal lists: swaps matrices for each time period
\item For multilayer networks: swaps within each layer
}

When the new network has different properties than the original (e.g., different
symmetry or weight type), the function updates the netify attributes accordingly.
For bipartite networks, the new network is always treated as asymmetric.

If the dyadic variable was originally specified with symmetry information via
\code{add_dyad_vars()}, that information is used unless overridden by the symmetric
parameter.
}
\examples{
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

}
\author{
Shahryar Minhas
}
