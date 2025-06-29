% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_helpers.R
\name{ggplot_add.netify_node}
\alias{ggplot_add.netify_node}
\title{Add netify_node to ggplot}
\usage{
\method{ggplot_add}{netify_node}(object, plot, object_name)
}
\arguments{
\item{object}{A netify_node object created by \code{\link{netify_node}}}

\item{plot}{A ggplot object to which the node layer will be added}

\item{object_name}{The name of the object being added (used internally by ggplot2)}
}
\value{
A ggplot object with the node layer added
}
\description{
S3 method to add netify_node objects to ggplot objects. This method is called
automatically when using the + operator with a netify_node object.
}
