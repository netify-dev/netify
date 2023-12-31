% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/input_checks.R
\name{actor_check}
\alias{actor_check}
\title{actor_check}
\usage{
actor_check(actor1, actor2, dyad_data)
}
\arguments{
\item{actor1}{user inputted object denoting
actor1 variable in data.frame}

\item{actor2}{user inputted object denoting
actor2 variable in data.frame}

\item{dyad_data}{data.frame in which actor1 and actor2
values are located}
}
\value{
returns a NULL object but stops the process
if there is an error detected
}
\description{
Checks to make sure that the actor fields
are populated and that they do not contain NAs
or non-character values
}
\author{
Ha Eun Choi, Shahryar Minhas
}
