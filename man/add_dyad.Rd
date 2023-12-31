% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_dyad.R
\name{add_dyad}
\alias{add_dyad}
\title{Add dyad data to a netify object}
\usage{
add_dyad(
  netlet,
  dyad_data,
  actor1 = NULL,
  actor2 = NULL,
  time = NULL,
  dyad_vars = NULL,
  dyad_vars_symmetric = NULL,
  replace_existing = FALSE
)
}
\arguments{
\item{netlet}{a netify object}

\item{dyad_data}{a dataframe object}

\item{actor1}{character: actor 1 in the data}

\item{actor2}{character: actor 2 in the data}

\item{time}{a character object indicating which variable in dyad_data tracks time}

\item{dyad_vars}{a vector of which variables from dyad_data should be merged}

\item{dyad_vars_symmetric}{logical vector: whether ties are symmetric, default is TRUE}

\item{replace_existing}{a logical indicating whether to replace existing nodal data}
}
\value{
a netify object
}
\description{
\code{add_dyad} takes in a dataframe and outputs a netify object.
}
\examples{
 
data(icews)

# cross-sectional case
icews_10 <- icews[icews$year==2010,]

verbCoop_net <- netify(
  dyad_data=icews_10,
  actor1 = 'i', actor2 = 'j', 
  symmetric=FALSE, weight='verbCoop' )

verbCoop_net <- add_dyad(
  netlet=verbCoop_net, 
  dyad_data=icews_10, 
  actor1='i', actor2='j', 
  dyad_vars=c('matlCoop', 'verbConf', 'matlConf'),
  dyad_vars_symmetric = rep(FALSE, 3) )

# dyadic data is stored in the dyad_data attribute
# as an array, it can be accessed in the following way:
dyad_array <- attr(verbCoop_net, 'dyad_data')[[1]]
dim(dyad_array)

# the dimensions of the array are: nr x nc x pd, where
# nr is the number of row actors, nc is the number of column actors,
# and pd is the number of dyadic variables

# longitudinal case
verbCoop_longit_net <- netify(
    dyad_data=icews, 
    actor1='i', actor2='j', time='year',
    symmetric=FALSE,
    weight='verbCoop' )

verbCoop_longit_net <- add_dyad(
    netlet=verbCoop_longit_net,
    dyad_data=icews, 
    actor1='i', actor2='j', time='year',
    dyad_vars = c('matlCoop', 'verbConf', 'matlConf'),
    dyad_vars_symmetric=rep(FALSE, 3) )

# dyadic data in the longit case is still stored in
# the dyad_data attribute but now as a list of arrays, 
# it can be accessed in the following way:
dyad_array_list <- attr(verbCoop_longit_net, 'dyad_data')
dim(dyad_array_list[['2002']])

# the names of the list elements correspond to the time
# periods and each array within the list is of the same
# dimensions as the array in the cross-sectional case

}
\author{
Cassy Dorff, Colin Henry, Shahryar Minhas
}
