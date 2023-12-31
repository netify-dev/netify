% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prep_for_igraph.R
\name{add_dyad_to_igraph}
\alias{add_dyad_to_igraph}
\title{add_dyad_to_igraph}
\usage{
add_dyad_to_igraph(netlet, dyad_data_list, igraph_object, time = NULL)
}
\arguments{
\item{netlet}{netify object}

\item{dyad_data_list}{dyad data from netlet object}

\item{igraph_object}{igraph object to modify}

\item{time}{time indicator for longit case}
}
\value{
igraph object with dyad attributes added
}
\description{
Add dyad attributes to an igraph object from netify object
}
\author{
Shahryar Minhas
}
