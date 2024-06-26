% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/peek.R
\name{peek}
\alias{peek}
\title{peek method for netify objects}
\usage{
peek(
  netlet,
  what_to_peek = 3,
  what_rows_to_peek = what_to_peek,
  what_cols_to_peek = what_to_peek,
  when_to_peek = 1,
  what_layers_to_peek = NULL
)
}
\arguments{
\item{netlet}{object of class netify}

\item{what_to_peek}{enter the name of specific nodes to peek in character vector form or provide a numeric range, default is to show the first three rows and columns of interactions}

\item{what_rows_to_peek}{similar as what_to_peek but specific to rows, default value is set to what_to_peek. If you want to peek at all rows then set this to NULL.}

\item{what_cols_to_peek}{similar as what_to_peek but specific to columns, default value is set to what_to_peek. If you want to peek at all columns then set this to NULL.}

\item{when_to_peek}{choose time points to peek from, default is to show the first time point of data.
If the entry is a numeric value or vector then it will be used as an index to the time dimension.
If the entry is a character vector then it will be used to match the time dimension labels.
If you want to peek at all time points then set this to NULL.}

\item{what_layers_to_peek}{if the netlet object has multiple layers, then you must choose one layer to peek at.}
}
\value{
slice of the network
}
\description{
peek takes in a netify object
and displays the some specified rows of information.
}
\examples{

# load data
data(icews)

# subset to a particular year
icews_10 <- icews[icews$year=='2010', ]

# gen netify object
icews_verbCoop <- netify(
  dyad_data=icews_10, actor1='i', actor2='j',
  symmetric=FALSE, weight='verbCoop' )

# peek at relations between a few countries
peek(icews_verbCoop,
	what_to_peek = c('United Kingdom', 'United States','France') )

# specify rows and cols to peek at
peek(icews_verbCoop,
	what_rows_to_peek = c('United Kingdom', 'United States','France'),
	what_cols_to_peek = c('Russian Federation', 'Sri Lanka') )

# peek with longit array
icews_matlConf <- netify(
	dyad_data=icews, 
	actor1='i', actor2='j', time='year',
	symmetric=FALSE, weight='matlConf',
	output_format = 'longit_array' )
# peek at a few years for the first three rows/cols, 
# specify numeric index or character refs
peek(icews_matlConf, when_to_peek=c(1, 5, 10))
peek(icews_matlConf, when_to_peek=c('2002', '2006', '2011'))


}
\author{
Cassy Dorff, Shahryar Minhas
}
