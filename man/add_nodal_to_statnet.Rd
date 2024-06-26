% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prep_for_network.R
\name{add_nodal_to_statnet}
\alias{add_nodal_to_statnet}
\title{add_nodal_to_statnet}
\usage{
add_nodal_to_statnet(netlet, node_data, statnet_object, time = NULL)
}
\arguments{
\item{netlet}{netify object}

\item{node_data}{node data from netlet object}

\item{statnet_object}{network object to modify}

\item{time}{time indicator for longit case}
}
\value{
statnet network object with nodal attributes added
}
\description{
Add nodal attributes to a network object from netify object
}
\author{
Shahryar Minhas
}
