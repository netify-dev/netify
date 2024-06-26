% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/decompose_netlet.R
\name{decompose_netlet}
\alias{decompose_netlet}
\title{Decompose a netify object into edges and nodal data frames}
\usage{
decompose_netlet(netlet, remove_zeros = TRUE)
}
\arguments{
\item{netlet}{A netify object to be decomposed.}

\item{remove_zeros}{Logical. If TRUE, remove edges with zero values.}
}
\value{
A list containing two elements: \code{edge_data} and \code{nodal_data}.
\code{edge_data} is a data frame of edges with attributes, and
\code{nodal_data} is a data frame containing node attributes.
}
\description{
\code{decompose_netlet} separates a netify object into its constituent parts:
a data frame of edges and a data frame of nodal attributes. This function
is particularly useful for preparing network data for analyses that
require separate edge and node data sets.
}
\examples{
# load icews data
data(icews)

# choose attributes
nvars = c( 'i_polity2', 'i_log_gdp', 'i_log_pop' )
dvars = c( 'matlCoop', 'verbConf', 'matlConf' )

# create a netify object
netlet = netify(
    dyad_data=icews, actor1='i', actor2='j',
    time = 'year',
    symmetric=FALSE, weight='verbCoop',
    mode='unipartite', sum_dyads=FALSE,
    actor_time_uniform=TRUE, actor_pds=NULL,
    diag_to_NA=TRUE, missing_to_zero=TRUE,
    nodal_vars = nvars, 
    dyad_vars = dvars
)

# decompose the netify object
decomposed = decompose_netlet( netlet )

lapply(decomposed, head)

}
\author{
Cassy Dorff, Shahryar Minhas
}
