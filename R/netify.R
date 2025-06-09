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
#' @param dyad_vars_symmetric logical vector: whether ties are symmetric, default is to use the same choice as the symmetric argument
#'
#' @return a netify object
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
    output_format=NULL,  # Changed default to NULL
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

    # validate actors and mode-specific requirements
    actor_mode_check(dyad_data, actor1, actor2, mode)

    # if sum_dyads is set to TRUE then users need to input nodal_vars and dyad_vars
    # themselves after the network is generated using add_node_vars and add_dyad_vars
    if( sum_dyads==TRUE ){
        if( !is.null(nodal_vars) | !is.null(dyad_vars) ){
            cli::cli_alert_warning(
                "Warning: When sum_dyads is set to TRUE nodal and dyadic attributes cannot automatically be created
                using `netify`. Instead users need to add them afterwards using the `add_dyad_vars` and `add_node_vars`
                functions.")
            nodal_vars <- dyad_vars <- NULL } }

    # FIX: Determine output_format based on actual data if not specified
    if(is.null(output_format)){
        if(is.null(time)){
            output_format <- 'cross_sec'
        } else {
            # Check how many unique time periods exist
            n_time_periods <- length(unique(dyad_data[[time]]))
            
            if(n_time_periods == 1){
                output_format <- 'cross_sec'
                # Inform user
                cli::cli_alert_info(
                    "Time variable specified but only one time period found. Creating cross-sectional network."
                )
            } else {
                # Default to list format for multiple time periods
                output_format <- 'longit_list'
            }
        }
    }

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

        # add it in using add_node_vars function
        netlet <- add_node_vars( 
            netlet=netlet, node_data=nodeData, 
            actor=actor1, time=time, 
            node_vars=nodal_vars ) }

    # check if dyad vars specified and if so add them to the object
    if(!is.null(dyad_vars)){

        # spit out warning if dyad_vars_symmetric is left NULL
        if( is.null( dyad_vars_symmetric ) ){ 
            dyad_vars_symmetric = rep(symmetric, length(dyad_vars)) }

        # add dyad vars using add_dyad_vars
        netlet <- add_dyad_vars(
            netlet=netlet, dyad_data=dyad_data, 
            actor1=actor1, actor2=actor2, time=time, 
            dyad_vars=dyad_vars, 
            dyad_vars_symmetric=dyad_vars_symmetric ) }

    # return netlet object
    return(netlet)
}
