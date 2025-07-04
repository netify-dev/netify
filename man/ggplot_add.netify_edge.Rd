% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_helpers.R
\name{ggplot_add.netify_edge}
\alias{ggplot_add.netify_edge}
\title{Add netify_edge to ggplot}
\usage{
\method{ggplot_add}{netify_edge}(object, plot, object_name)
}
\arguments{
\item{object}{A netify_edge object created by \code{\link{netify_edge}}}

\item{plot}{A ggplot object to which the edge layer will be added}

\item{object_name}{The name of the object being added (used internally by ggplot2)}
}
\value{
A ggplot object with the edge layer added
}
\description{
S3 method to add netify_edge objects to ggplot objects. This method is called
automatically when using the + operator with a netify_edge object.
}
