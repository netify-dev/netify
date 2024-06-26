% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/layer_netlet.R
\name{layer_netlet}
\alias{layer_netlet}
\title{Layer netify objects together to create a multilayer network}
\usage{
layer_netlet(netlet_list, layer_labels = NULL)
}
\arguments{
\item{netlet_list}{a list of netifty objects that you want to layer together}

\item{layer_labels}{character: label of the layer for each netify object}
}
\value{
an a multilayer network of class netify
}
\description{
\code{layer_netlet} takes in two netify objects and
layers them together to create a multilayer network
}
\examples{

# cross-sectional example
data(icews)
icews_10 <- icews[icews$year==2010,]

# generate netify objects that will be layered together
icews_verbCoop <- netify(
    dyad_data=icews_10, actor1='i', actor2='j',
    symmetric=FALSE, weight='verbCoop',
    nodal_vars=c('i_log_gdp', 'i_log_pop'),
    dyad_vars=c('verbConf') )

icews_matlCoop <- netify(
    dyad_data=icews_10, actor1='i', actor2='j',
    symmetric=FALSE, weight='matlCoop',
    nodal_vars='i_polity2',
    dyad_vars=c('matlConf') )

# layer together cross-sec netify objects together
icews_verbCoop_matlCoop <- layer_netlet(
    netlet_list=list(icews_verbCoop, icews_matlCoop),
    layer_labels=c('verbCoop', 'matlCoop') )

# dimensions of the multilayer network from the
# cross-sectional case will be a 
# (number of actors) x (number of actors) x (number of layers)
dim(get_raw(icews_verbCoop_matlCoop))

# longitudinal array example
icews_verbCoop_longit_a <- netify(
    dyad_data=icews, actor1='i', actor2='j', time='year',
    symmetric=FALSE, weight='verbCoop',
    nodal_vars=c('i_log_gdp', 'i_log_pop'),
    dyad_vars=c('verbConf'),
    output_format='longit_array' )
icews_matlCoop_longit_a <- netify(
    dyad_data=icews, actor1='i', actor2='j', time='year',
    symmetric=FALSE, weight='matlCoop',
    nodal_vars=c('i_polity2'),
    dyad_vars=c('matlConf'),
    output_format='longit_array' )

# layer together
icews_verbCoop_matlCoop_longit_a <- layer_netlet(
    netlet_list=list(icews_verbCoop_longit_a, icews_matlCoop_longit_a),
    layer_labels=c('verbCoop', 'matlCoop') )

# dimensions of the multilayer network from the
# longitudinal array case will be a 
# (number of actors) x (number of actors) x (number of layers) x 
# (number of time periods)
dim(get_raw(icews_verbCoop_matlCoop_longit_a)) 

# longitudinal list example
# generate similar longitudinal list versions
icews_verbCoop_longit_l <- netify(
    dyad_data=icews, actor1='i', actor2='j', time='year',
    symmetric=FALSE, weight='verbCoop',
    nodal_vars=c('i_log_gdp', 'i_log_pop'),
    dyad_vars=c('verbConf') )
icews_matlCoop_longit_l <- netify(
    dyad_data=icews, actor1='i', actor2='j', time='year',
    symmetric=FALSE, weight='matlCoop',
    nodal_vars=c('i_polity2'),
    dyad_vars=c('matlConf') )

# layer together
icews_verbCoop_matlCoop_longit_l <- layer_netlet(
    netlet_list=list(icews_verbCoop_longit_l, icews_matlCoop_longit_l),
    layer_labels=c('verbCoop', 'matlCoop') )

# dimensions of the multilayer network from the 
# longitudinal list case will be a
# (number of time periods) list of 
# (number of actors) x (number of actors) x (number of layers) arrays
names(get_raw(icews_verbCoop_matlCoop_longit_l))
dim(get_raw(icews_verbCoop_matlCoop_longit_l)$'2010')

# information on layer labels can be accessed 
# from  the `layers` attribute 
attr(icews_verbCoop_matlCoop, 'layers')
attr(icews_verbCoop_matlCoop_longit_l, 'layers')

}
\author{
Cassy Dorff, Shahryar Minhas
}
