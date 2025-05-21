#' Create network object from data.frame
#'
#' This function takes in a dyadic dataset and outputs a netlet object.
#'
#' @param dyad_data data object to netify
#' @param actor1 character: name of the actor 1 variable in the data
#' @param actor2 character: name of the actor 2 variable in the data
#' @param time character: name of the time variable in the data, if 
#' no time is provided then it will be assumed
#' @param weight character: name of the weighted edge variable in 
#' the data, default is NULL
#' @param symmetric logical: whether ties are symmetric, default is TRUE
#' @param mode character: whether the network is unipartite or bipartite, default is unipartite
#' @param sum_dyads logical: whether to sum up the `weight` value when there exists repeating dyads
#' @param actor_time_uniform logical: whether to assume
#' actors are the same across the full time series observed in the data
#' TRUE means that actors are the same across the full time
#' series observed in the data and the outputted netify object will
#' be in an array format.
#' FALSE means that actors come in and out of the observed data and
#' their "existence" should be determined by the data, meaning that
#' their first year of existence will be determined by the time point
#' of their first event and their last year of existence by the
#' time point of their last event. Outputted netify object will be
#' in a list format.
#' @param actor_pds a data.frame indicating start and end time point for every
#' actor, this can be created using `get_actor_time_info.R`, unless provided this will
#' estimated for the user based on their choice of `actor_time_uniform`
#' @param diag_to_NA logical: whether diagonals should be set to NA, default is TRUE
#' @param missing_to_zero logical: whether missing values should be set to zero, default is TRUE
#' @param output_format character: "cross_sec", "longit_array", or 
#' "longit_list. If not specified and time is NULL then output_format 
#' will be "cross_sec" and if time is specified then output_format 
#' will default to "longit_list".
#' @param nodal_vars character vector: names of the nodal variables in the dyad_data object that should be added as attributes to the netify object
#' @param dyad_vars character vector: names of the dyadic variables in the dyad_data
#' object that should be added as attributes to the netify object, 
#' default is to add all the variables from the extra_dyadic_data data.frame
#' @param dyad_vars_symmetric logical vector: whether ties are symmetric, default is to use the same choice as
#' the symmetric argument
#'
#' @return a netlet object
#'
#' @author Ha Eun Choi, Cassy Dorff, Colin Henry, Shahryar Minhas
#'
#' @examples
#'
#' # load example directed event data from ICEWS
#' # this data comes in the form of a dyadic
#' # dataframe where all dyad pairs are listed
#' data(icews)
#' 
#' # generate a longitudional, directed and weighted network
#' # where the weights are matlConf and results are organized
#' # in an array
#' icews_matlConf <- netify(
#'     dyad_data=icews,
#'     actor1='i', actor2='j', time='year',
#'     symmetric=FALSE, weight='matlConf')
#'
#' # generate a longitudional, directed and weighted network
#' # where the weights are matlConf and results are organized
#' # in an array and we have both dyadic and nodal attributes
#' icews_matlConf <- netify(
#'     dyad_data=icews,
#'     actor1='i', actor2='j', time='year',
#'     symmetric=FALSE, weight='matlConf',
#'     nodal_vars=c('i_polity2', 'i_log_gdp', 'i_log_pop'),
#'     dyad_vars=c('matlCoop', 'verbCoop', 'verbConf'),
#'     dyad_vars_symmetric=c(FALSE, FALSE, FALSE) )  
#' 
#'
#' @export netify
#' 

netify <- function(
    dyad_data, 
    actor1=NULL, actor2=NULL, time=NULL, 
    symmetric=TRUE, mode='unipartite',
    weight=NULL, sum_dyads=FALSE,
    actor_time_uniform=TRUE,
    actor_pds=NULL,
    diag_to_NA=TRUE,
    missing_to_zero=TRUE,    
    output_format=ifelse(
        is.null(time), 
        'cross_sec', 
        'longit_list'),
    nodal_vars=NULL,
    dyad_vars=NULL, 
    dyad_vars_symmetric=rep(
        symmetric, length(dyad_vars))
){

    # checks on user inputs
    dyad_data <- df_check(dyad_data)
    logical_check(sum_dyads, symmetric, diag_to_NA, missing_to_zero)
    actor_check(actor1, actor2, dyad_data)
    weight_check(weight, dyad_data)
    if(!is.null(time)){
        time_check(time, dyad_data) }

    # convert actor labels to character
    # check data type for actor1 and actor2
    if(!is.character(dyad_data[,actor1]) | !is.character(dyad_data[,actor2])){
        cli::cli_alert_warning(
            "Warning: Converting `actor1` and/or `actor2` to character vector(s).")
        dyad_data[,actor1] <- char(dyad_data[,actor1])
        dyad_data[,actor2] <- char(dyad_data[,actor2]) }

    # if sum_dyads is set to TRUE then users need to input nodal_vars and dyad_vars
    # themselves after the network is generated using add_nodal and add_dyad
    if( sum_dyads==TRUE ){
        if( !is.null(nodal_vars) | !is.null(dyad_vars) ){
            cli::cli_alert_warning(
                "Warning: When sum_dyads is set to TRUE nodal and dyadic attributes cannot automatically be created
                using `netify`. Instead users need to add them afterwards using the `add_dyad` and `add_nodal`
                functions.")
            nodal_vars <- dyad_vars <- NULL } }

    # choose relevant get_adjacency_* function
    # based on output_format
    if(output_format=='cross_sec'){
        netlet <- get_adjacency(
            dyad_data=dyad_data,
            actor1=actor1, actor2=actor2,
            symmetric=symmetric, mode=mode, 
            weight=weight,  
            sum_dyads=sum_dyads,
            diag_to_NA=diag_to_NA,
            missing_to_zero=missing_to_zero ) }

    #
    if(output_format=='longit_array'){
        netlet <- get_adjacency_array(
            dyad_data=dyad_data,
            actor1=actor1, actor2=actor2, time=time,
            symmetric=symmetric, mode=mode,
            weight=weight, sum_dyads=sum_dyads,
            diag_to_NA=diag_to_NA, missing_to_zero=missing_to_zero ) }

    #
    if(output_format=='longit_list'){
        netlet <- get_adjacency_list(
            dyad_data=dyad_data,
            actor1=actor1, actor2=actor2, time=time,
            symmetric=symmetric, mode=mode,
            weight=weight, sum_dyads=sum_dyads,
            actor_time_uniform=actor_time_uniform,
            actor_pds=actor_pds,
            diag_to_NA=diag_to_NA, missing_to_zero=missing_to_zero ) }

    # add attributes if they were provided by user
    # check if nodal vars specified and if so add them to the object
    if(!is.null(nodal_vars)){

        # pull out nodal data
        nodeData <- unique(dyad_data[,c(actor1, time, nodal_vars)])

        # add it in using add_nodal function
        netlet <- add_nodal( 
            netlet=netlet, node_data=nodeData, 
            actor=actor1, time=time, 
            node_vars=nodal_vars ) }

    # check if dyad vars specified and if so add them to the object
    if(!is.null(dyad_vars)){

        # spit out warning if dyad_vars_symmetric is left NULL
        if( is.null( dyad_vars_symmetric ) ){ 
            dyad_vars_symmetric = rep(symmetric, length(dyad_vars)) }

        # add dyad vars using add_dyad
        netlet <- add_dyad(
            netlet=netlet, dyad_data=dyad_data, 
            actor1=actor1, actor2=actor2, time=time, 
            dyad_vars=dyad_vars, 
            dyad_vars_symmetric=dyad_vars_symmetric ) }

    # return netlet object
    return(netlet)
}


#' Constructs a generic netlet Object
#'
#' `new_netify` is a low-level constructor for efficiently creating new netlet objects
#' Not visible to users.
#'
#' @param data data object
#' @param ... additional parameters
#'
#' @return a netlet object
#'
#' @author Ha Eun Choi, Cassy Dorff, Colin Henry, Shahryar Minhas

new_netify <- function(data, ... ) {

    # get list of default params
    default_params <- list(...)

    # append to existing attributes
    attributes(data) <- c( attributes(data), default_params )
    netify_type <- switch( class(data)[1],
        "matrix" = "cross_sec",
        "array" = "longit_array",
        "list" = "longit_list" )

    # if dealing with a list object then add attributes
    # to every matrix object in list
    if(netify_type == "longit_list"){
        for(ii in 1:length(data)){
            mat_slice <- data[[ii]]
            attributes(mat_slice) <- c(
                attributes(mat_slice), default_params )
            attr(mat_slice, 'netify_type') <- 'cross_sec'
            class(mat_slice) <- 'netify'            
            data[[ii]] <- mat_slice
        }
    }

    # convert structure of out to netify type
    out <- structure( data,
        netify_type = netify_type,
        class = "netify" )

    return(out)
}

#' Is this object a netify object?
#' 
#' @aliases is.netify
#' @param x An R object
#' @return Logical constant, \code{TRUE} if argument \code{object} is a netify
#' object
#' @author Colin Henry
#' 
#' @keywords netify
#' 
#' @export is_netify

is_netify <- function(x){
  "netify" %in% class(x)
}

#' netify_check
#'
#' Checks to make sure that object is of class netify
#' and stops process if not
#' @param netlet user inputted object to check
#' @return NULL object but stops the process if there 
#' is an error detected
#' @author Ha Eun Choi, Colin Henry, Shahryar Minhas
#' 
#' @export netify_check

netify_check <- function(netlet){

    # check if `dyad_data` is df
    if(!is_netify(netlet)) {
        cli::cli_alert_danger("Error: check data type. Inputted object is not a `netify` object.")
        stop() }

    # 
    return(invisible(NULL)) }
