# library(netify)

# # load example directed event data from ICEWS
# # this data comes in the form of a dyadic
# # dataframe where all dyad pairs are listed
# data(icews)

# #'
# # generate a longitudional, directed and weighted network
# # where the weights are matlConf and results are organized
# # in an array and we have both dyadic and nodal attributes
# net_adj <- netify(
# 	dyad_data=icews[icews$year==2010,],
# 	actor1='i', actor2='j', 
# 	symmetric=FALSE, weight='matlConf',
# 	nodal_vars=c('i_polity2', 'i_log_gdp', 'i_log_pop'),
# 	dyad_vars=c('matlCoop', 'verbCoop', 'verbConf'),
# 	dyad_vars_symmetric=c(FALSE, FALSE, FALSE) )  

# net_list <- netify(
# 	dyad_data=icews,
# 	actor1='i', actor2='j', time='year',
# 	symmetric=FALSE, weight='matlConf',
# 	nodal_vars=c('i_polity2', 'i_log_gdp', 'i_log_pop'),
# 	dyad_vars=c('matlCoop', 'verbCoop', 'verbConf'),
# 	dyad_vars_symmetric=c(FALSE, FALSE, FALSE) )  

# net_arr <- netify(
# 	dyad_data=icews,
# 	actor1='i', actor2='j', time='year',
# 	symmetric=FALSE, weight='matlConf',
# 	nodal_vars=c('i_polity2', 'i_log_gdp', 'i_log_pop'),
# 	dyad_vars=c('matlCoop', 'verbCoop', 'verbConf'),
# 	dyad_vars_symmetric=c(FALSE, FALSE, FALSE),
#     output_format='longit_array'
#     )  

# # generate netify objects that will be layered together
# icews_10 <- icews[icews$year==2010,]
# icews_verbCoop <- netify(
#     dyad_data=icews_10, actor1='i', actor2='j',
#     symmetric=FALSE, weight='verbCoop',
#     nodal_vars=c('i_log_gdp', 'i_log_pop'),
#     dyad_vars=c('verbConf') )

# icews_matlCoop <- netify(
#     dyad_data=icews_10, actor1='i', actor2='j',
#     symmetric=FALSE, weight='matlCoop',
#     nodal_vars='i_polity2',
#     dyad_vars=c('matlConf') )    

# # layer together cross-sec netify objects together
# multilayer <- layer_netify(
#     netlet_list=list(icews_verbCoop, icews_matlCoop),
#     layer_labels=c('verbCoop', 'matlCoop') ) 

# # longitudinal list example
# # generate similar longitudinal list versions
# icews_verbCoop_longit_l <- netify(
#     dyad_data=icews, actor1='i', actor2='j', time='year',
#     symmetric=FALSE, weight='verbCoop',
#     nodal_vars=c('i_log_gdp', 'i_log_pop'),
#     dyad_vars=c('verbConf') )

# icews_matlCoop_longit_l <- netify(
#     dyad_data=icews, actor1='i', actor2='j', time='year',
#     symmetric=FALSE, weight='matlCoop',
#     nodal_vars=c('i_polity2'),
#     dyad_vars=c('matlConf') )

# # layer together
# multilayer_longit <- layer_netify(
#     netlet_list=list(icews_verbCoop_longit_l, icews_matlCoop_longit_l),
#     layer_labels=c('verbCoop', 'matlCoop') )


# ######
# ######
# ######
# ######

# netlet = net_list

# # user input checks
# netify_check(netlet)

# # pull out attrs and msrmnts of original
# objAttrs <- attributes(netlet)
# msrmnts <- netify_measurements(netlet)
# nlayers <- ifelse(is.null(msrmnts$n_layers), 1, msrmnts$n_layers)
# ntime <- ifelse(is.null(msrmnts$n_time), 1, msrmnts$n_time)
# symm <- objAttrs$symmetric
# diag_to_NA <- objAttrs$diag_to_NA

# # nodal_data
# nvars <- msrmnts$nvars
# nodal <- attr(netlet, 'nodal_data')

# # dyad_data
# dvars <- msrmnts$dvars
# dyad <- attr(netlet, 'dyad_data')
# dyad <- reshape2::melt(dyad)

# # recast data so that the variables in
# # var3 of dyad are going across columns
# dyad <- reshape2::dcast(dyad, Var1 + Var2 + L1 ~ Var3, value.var='value')
# if(diag_to_NA){ dyad <- dyad[dyad$Var1 != dyad$Var2,] }

# # graph_data

# # net_data
# net <- get_raw(netlet)
# net <- reshape2::melt(net)
# if(diag_to_NA){ net <- net[net$Var1 != net$Var2,] }
# if(nlayers>1 & ntime==1){
#     net <- reshape2::dcast(net, Var1 + Var2 ~ Var3, value.var='value')
# }
# if(nlayers>1 & ntime>1){
#     net <- reshape2::dcast(net, Var1 + Var2 + L1 ~ Var3, value.var='value')
# }

# # add id variables for cross-sectional case
# if(ntime==1){
    
#     # ids for net variable
#     net$id_row <- net$Var1
#     net$id_col <- net$Var2
#     net$did <- paste0(net$Var1, '_', net$Var2)

#     # ids for nodal data
#     nodal$id <- nodal$actor

#     # ids for dyad data
#     dyad$did <- paste0(dyad$Var1, '_', dyad$Var2)
# }

# # add id variables for longit case
# if(ntime > 1){

#     # ids for net variable
#     net$id_row <- paste0(net$Var1, '_', net$L1)
#     net$id_col <- paste0(net$Var2, '_', net$L1)
#     net$did <- paste0(net$Var1, '_', net$Var2, '_', net$L1)

#     # ids for nodal data
#     nodal$id <- paste0(nodal$actor, '_', nodal$time)

#     # ids for dyad data
#     dyad$did <- paste0(dyad$Var1, '_', dyad$Var2, '_', dyad$L1)
# }

# #
# head(nodal, n=3)
# head(dyad, n=3)
# head(net, n=3)

# # merge nodal vars in based on symm condition
# if(!symm){
#     row <- nodal[match(net$id_row, nodal$id),nvars]
#     names(row) = paste0(nvars, '_row')
#     col <- nodal[match(net$id_row, nodal$id),nvars]
#     names(col) = paste0(nvars, '_col')
#     net <- cbind(net, row, col)
# }
# if(symm){
#     actor <- nodal[match(net$id_row, nodal$id),nvars]
#     net <- cbind(net, actor)
# }

# # merge dyad vars in
# dyad <- dyad[match(net$did, dyad$did),dvars]
# net <- cbind(net, dyad)
