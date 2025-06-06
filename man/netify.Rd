% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/netify.R
\name{netify}
\alias{netify}
\title{Create network object from data.frame}
\usage{
netify(
  dyad_data,
  actor1 = NULL,
  actor2 = NULL,
  time = NULL,
  symmetric = TRUE,
  mode = "unipartite",
  weight = NULL,
  sum_dyads = FALSE,
  actor_time_uniform = TRUE,
  actor_pds = NULL,
  diag_to_NA = TRUE,
  missing_to_zero = TRUE,
  output_format = ifelse(is.null(time), "cross_sec", "longit_list"),
  nodal_vars = NULL,
  dyad_vars = NULL,
  dyad_vars_symmetric = rep(symmetric, length(dyad_vars))
)
}
\arguments{
\item{dyad_data}{data object to netify}

\item{actor1}{character: name of the actor 1 variable in the data}

\item{actor2}{character: name of the actor 2 variable in the data}

\item{time}{character: name of the time variable in the data, if
no time is provided then it will be assumed}

\item{symmetric}{logical: whether ties are symmetric, default is TRUE}

\item{mode}{character: whether the network is unipartite or bipartite, default is unipartite}

\item{weight}{character: name of the weighted edge variable in
the data, default is NULL}

\item{sum_dyads}{logical: whether to sum up the \code{weight} value when there exists repeating dyads}

\item{actor_time_uniform}{logical: whether to assume
actors are the same across the full time series observed in the data
TRUE means that actors are the same across the full time
series observed in the data and the outputted netify object will
be in an array format.
FALSE means that actors come in and out of the observed data and
their "existence" should be determined by the data, meaning that
their first year of existence will be determined by the time point
of their first event and their last year of existence by the
time point of their last event. Outputted netify object will be
in a list format.}

\item{actor_pds}{a data.frame indicating start and end time point for every
actor, this can be created using \code{get_actor_time_info.R}, unless provided this will
estimated for the user based on their choice of \code{actor_time_uniform}}

\item{diag_to_NA}{logical: whether diagonals should be set to NA, default is TRUE}

\item{missing_to_zero}{logical: whether missing values should be set to zero, default is TRUE}

\item{output_format}{character: "cross_sec", "longit_array", or
"longit_list. If not specified and time is NULL then output_format
will be "cross_sec" and if time is specified then output_format
will default to "longit_list".}

\item{nodal_vars}{character vector: names of the nodal variables in the dyad_data object that should be added as attributes to the netify object}

\item{dyad_vars}{character vector: names of the dyadic variables in the dyad_data
object that should be added as attributes to the netify object,
default is to add all the variables from the extra_dyadic_data data.frame}

\item{dyad_vars_symmetric}{logical vector: whether ties are symmetric, default is to use the same choice as
the symmetric argument}
}
\value{
a netify object
}
\description{
This function takes in a dyadic dataset and outputs a netlet object.
}
\examples{

# load example directed event data from ICEWS
# this data comes in the form of a dyadic
# dataframe where all dyad pairs are listed
data(icews)

# generate a longitudional, directed and weighted network
# where the weights are matlConf and results are organized
# in an array
icews_matlConf <- netify(
    dyad_data=icews,
    actor1='i', actor2='j', time='year',
    symmetric=FALSE, weight='matlConf')

# generate a longitudional, directed and weighted network
# where the weights are matlConf and results are organized
# in an array and we have both dyadic and nodal attributes
icews_matlConf <- netify(
    dyad_data=icews,
    actor1='i', actor2='j', time='year',
    symmetric=FALSE, weight='matlConf',
    nodal_vars=c('i_polity2', 'i_log_gdp', 'i_log_pop'),
    dyad_vars=c('matlCoop', 'verbCoop', 'verbConf'),
    dyad_vars_symmetric=c(FALSE, FALSE, FALSE) )  


}
\author{
Ha Eun Choi, Cassy Dorff, Colin Henry, Shahryar Minhas
}
