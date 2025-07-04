% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_adjacency_list.R
\name{get_adjacency_list}
\alias{get_adjacency_list}
\title{Create a netify list from longitudinal dyadic data}
\usage{
get_adjacency_list(
  dyad_data,
  actor1 = NULL,
  actor2 = NULL,
  time = NULL,
  symmetric = TRUE,
  mode = "unipartite",
  weight = NULL,
  sum_dyads = FALSE,
  actor_time_uniform = FALSE,
  actor_pds = NULL,
  diag_to_NA = TRUE,
  missing_to_zero = TRUE,
  nodelist = NULL
)
}
\arguments{
\item{dyad_data}{A data.frame containing longitudinal dyadic observations. Will
be coerced to data.frame if a tibble or data.table is provided.}

\item{actor1}{Character string specifying the column name for the first actor
in each dyad.}

\item{actor2}{Character string specifying the column name for the second actor
in each dyad.}

\item{time}{Character string specifying the column name for time periods.}

\item{symmetric}{Logical. If TRUE (default), treats the network as undirected
(i.e., edges have no direction). If FALSE, treats the network as directed.}

\item{mode}{Character string specifying network structure. Options are:
\itemize{
\item \code{"unipartite"}: One set of actors (default)
\item \code{"bipartite"}: Two distinct sets of actors
}}

\item{weight}{Character string specifying the column name containing edge weights.
If NULL (default), edges are treated as unweighted (binary).}

\item{sum_dyads}{Logical. If TRUE, sums weight values when multiple edges exist
between the same actor pair in the same time period. If FALSE (default), uses
the last observed value.}

\item{actor_time_uniform}{Logical indicating how to handle actor composition:
\itemize{
\item \code{TRUE}: Assumes all actors exist across the entire time range
\item \code{FALSE}: Determines actor existence from the data - actors exist
from their first observed interaction to their last
}}

\item{actor_pds}{Optional data.frame specifying when actors enter and exit the
network. Must contain columns 'actor', 'min_time', and 'max_time'. Can be
created using \code{get_actor_time_info()}. If provided, overrides actor_time_uniform.}

\item{diag_to_NA}{Logical. If TRUE (default), sets diagonal values (self-loops)
to NA. Automatically set to FALSE for bipartite networks.}

\item{missing_to_zero}{Logical. If TRUE (default), treats missing edges as zeros.
If FALSE, missing edges remain as NA.}

\item{nodelist}{Character vector of actor names to include in the network.
If provided, ensures all listed actors appear in the network even if they
have no edges (isolates). Useful when working with edgelists that only
contain active dyads.}
}
\value{
A list of class "netify" (a netify list) with:
\itemize{
\item \strong{Elements}: Named list where each element is a netify matrix
for one time period
\item \strong{Names}: Character representation of time periods
\item \strong{Class}: "netify" - this is a full netify object compatible
with all netify functions
\item \strong{Attributes}: Extensive metadata including network properties,
actor composition information, and processing parameters
}

Each matrix in the list may have different dimensions if actor composition
varies over time. The returned object can be used with all netify functions
such as \code{summary()}, \code{plot()}, \code{to_igraph()}, etc.
}
\description{
\code{get_adjacency_list} converts longitudinal dyadic data into a list of adjacency
matrices of class "netify". This function creates a list structure where each
element is a network matrix for a specific time period, allowing for
time-varying actor composition.
}
\details{
\strong{Note on usage:}

While this function is exported and available for direct use, the primary and
recommended way to create netify objects from longitudinal dyadic data is through
the \code{netify()} function. The \code{netify()} function:
\itemize{
\item Automatically chooses between array and list representations based on
your data
\item Provides more comprehensive data validation
\item Can incorporate nodal and dyadic attributes during creation
\item Offers a unified interface for all types of network data
}

Use \code{get_adjacency_list()} directly only when you specifically need a list
structure or require low-level control over the creation process.

\strong{Actor composition handling:}

This function is particularly useful when actors enter and exit the network
over time. Unlike \code{get_adjacency_array()}, which requires constant actor
composition, this function can handle:
\itemize{
\item New actors appearing in later time periods
\item Actors exiting and no longer appearing in the data
\item Different sets of actors active in each time period
}
}
\examples{
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

}
\author{
Cassy Dorff, Ha Eun Choi, Shahryar Minhas
}
