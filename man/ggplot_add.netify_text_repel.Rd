% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_helpers.R
\name{ggplot_add.netify_text_repel}
\alias{ggplot_add.netify_text_repel}
\title{Add netify_text_repel to ggplot}
\usage{
\method{ggplot_add}{netify_text_repel}(object, plot, object_name)
}
\arguments{
\item{object}{A netify_text_repel object created by \code{\link{netify_text_repel}}}

\item{plot}{A ggplot object to which the text_repel layer will be added}

\item{object_name}{The name of the object being added (used internally by ggplot2)}
}
\value{
A ggplot object with the text_repel layer added
}
\description{
S3 method to add netify_text_repel objects to ggplot objects. This method is called
automatically when using the + operator with a netify_text_repel object.
}
