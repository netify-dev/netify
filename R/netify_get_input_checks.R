# user input checks for package functions

#' df_check
#'
#' Checks to make sure a data.frame is inputted and
#' if a `tibble` or `data.table` data.frame is inputted then it is
#' converted to a base R data.frame object
#' @param df user inputted object to check
#' @param msg msg to user if df check fails
#' @return data.frame object
#' @author Ha Eun Choi, Shahryar Minhas
#'
#' @keywords internal
#' @noRd

df_check <- function(
    df,
    msg="Error: check data type. `dyad_data` is not a dataframe."
    ){

    # check if `dyad_data` is df
    if(!inherits(df, "data.frame")) { 
        cli::cli_alert_danger(msg)
        stop() }

    # convert to base R data.frame since we rely on base subsetting
    df <- as.data.frame( df, stringsAsFactors=FALSE )
    
    # check for empty dataset
    if(nrow(df) == 0) {
        cli::cli_abort(
            c("x" = "Cannot create network from empty dataset.",
              "i" = "dyad_data has {nrow(df)} rows.",
              "!" = "Please provide a dataset with at least one dyadic observation.")
        )
    }
    
    # return
    return(df) }

#' logical_check
#'
#' Checks to make sure user has correctly inputted logicals
#' for select inputs
#' @param sum_dyads user supplied input
#' @param symmetric user supplied input
#' @param diag_to_NA user supplied input
#' @param missing_to_zero user supplied input
#' @param actor_time_uniform optional user supplied input
#' @return returns a NULL object but stops the process
#' if there is an error detected
#' @author Ha Eun Choi, Shahryar Minhas
#'
#' @keywords internal
#' @noRd

logical_check <- function(
    sum_dyads, symmetric, 
    diag_to_NA, missing_to_zero, 
    actor_time_uniform = NULL
    ){

        # check data type for sum_dyads
        if(!is.logical(sum_dyads)){
            cli::cli_alert_danger("Error: check data type. `sum_dyads` is not a logical.")
            stop() }        

        # check data type for symmetric
        if(!is.logical(symmetric)){
            cli::cli_alert_danger("Error: check data type. `symmetric` is not a logical.")
            stop() }

        # check data type for diag_to_NA
        if(!is.logical(diag_to_NA)){
            cli::cli_alert_danger("Error: check data type. `diag_to_NA` is not a logical.")
            stop() }

        # check data type for missing_to_zero
        if(!is.logical(missing_to_zero)){
            cli::cli_alert_danger("Error: check data type. `missing_to_zero` is not a logical.")
            stop() }

        # check actor_time_uniform
        if(!is.null(actor_time_uniform)){
            if(!is.logical(actor_time_uniform)){
                cli::cli_alert_danger("Error: check data type. `actor_time_uniform` is not a logical.")
                stop() }
        }

        #
        return(invisible(NULL))
    }

#' actor_check
#'
#' Checks to make sure that the actor fields
#' are populated and that they do not contain NAs
#' or non-character values
#' @param actor1 user inputted object denoting 
#' actor1 variable in data.frame
#' @param actor2 user inputted object denoting 
#' actor2 variable in data.frame
#' @param dyad_data data.frame in which actor1 and actor2
#' values are located
#' @return returns a NULL object but stops the process
#' if there is an error detected
#' @author Ha Eun Choi, Shahryar Minhas
#'
#' @keywords internal
#' @noRd

actor_check <- function(actor1, actor2, dyad_data){

    # check if the fields were populated
    if(is.null(actor1) | is.null(actor2)){
        cli::cli_alert_danger("Error: `actor1` or `actor2` values are required.")
        stop() }

    # check to make sure that actor1 and actor2 variables exists in data
    if(!actor1 %in% colnames(dyad_data) | !actor2 %in% colnames(dyad_data)){
        cli::cli_alert_danger("Error: `actor1` and/or `actor2` variables do not exist in the `dyad_data` object.")
        stop() }

    # check if actor1 or actor2 contain missing values
    # and if they do give a missing warning
    if(any(is.na(dyad_data[,actor1])) | any(is.na(dyad_data[,actor2])) ){
        cli::cli_alert_danger(
            "Error: `actor1` and/or `actor2` contains missing value(s)")
        stop() }

    #
    return(invisible(NULL))
}

#' weight_check
#'
#' Checks to make sure that the weight field
#' is populated correctly
#' @param weight user inputted object for weight
#' and NULL by default
#' @param dyad_data data.frame in which weight
#' values are located
#' @return returns a NULL object but stops the process
#' if there is an error detected
#' @author Ha Eun Choi, Shahryar Minhas
#'
#' @keywords internal
#' @noRd

weight_check <- function(weight, dyad_data){

    # check data type for weight
    if(!is.null(weight) & !is.character(weight)){
        cli::cli_alert_danger("Error: check data type. `weight` should be left NULL or a character value referring to a variable from the dyad_data object should be provided.")
        stop() }    

    # check to make sure that weight variable exists in data
    if(!is.null(weight)){
        if(!weight %in% colnames(dyad_data)){
            cli::cli_alert_danger("Error: `weight` variable does not exist in the `dyad_data` object.")
        stop() }}

    #
    return(invisible(NULL))
}


#' weight_string_label
#' 
#' Create attribute label of for weight
#' based on user inputs to netify
#' 
#' @param weight user input for weight
#' @param sum_dyads logical user input for sum_dyads
#' @return character string
#' @author Shahryar Minhas
#'
#' @keywords internal
#' @noRd

weight_string_label <- function(weight, sum_dyads){

    # weight NULL & sum_dyads FALSE
    if( is.null(weight) & !sum_dyads ){
        weight_string <- 'Binary Weights'
    }

    # weight NULL & sum_dyads TRUE
    if( is.null(weight) & sum_dyads ){
        weight_string <- 'Sum of Binary Weights'
    }

    # weight not NULL & sum_dyads FALSE
    if( !is.null(weight) & !sum_dyads ){
        weight_string <- paste0(
            'Weights from `', weight, '`')
    }

    # weight not NULL & sum_dyads TRUE
    if( !is.null(weight) & sum_dyads ){
        weight_string <- paste0(
            'Sum of Weights from `', weight,'`')
    }

    #
    return(weight_string) }

#' time_check
#'
#' Checks to make sure that the time field
#' is populated correctly
#' @param time user inputted object for weight
#' and NULL by default
#' @param dyad_data data.frame in which time
#' values are located
#' @return returns a NULL object but stops the process
#' if there is an error detected
#' @author Ha Eun Choi, Shahryar Minhas
#'
#' @keywords internal
#' @noRd

time_check <- function(time, dyad_data){

    # check data type
    if(!is.character(time)){
        cli::cli_alert_danger("Error: check data type. `time` should be left NULL or a character value referring to a variable from the dyad_data object should be provided.")
        stop() }

    # check to make sure that time variable exists in data
    if(!time %in% colnames(dyad_data)){
        cli::cli_alert_danger("Error: `time` variable does not exist in the `dyad_data` object.")
        stop() }
    
    # 
    time_data <- dyad_data[[time]]
    
    # numeric, Date, POSIXct, POSIXlt, and character
    if(!inherits(time_data, c("numeric", "integer", "Date", "POSIXct", "POSIXlt", "character"))){
        cli::cli_alert_danger(
            "Error: Time variable must be numeric, Date, POSIXct, POSIXlt, or character. Found class: {class(time_data)[1]}"
        )
        stop()
    }
    
    # if charac, check if it can be reasonably sorted
    if(is.character(time_data)){
        # 
        sample_vals <- utils::head(unique(time_data), 10)
        cli::cli_alert_info(
            "Time variable is character. Will sort alphabetically. Sample values: {paste(sample_vals, collapse=', ')}"
        )
    }

    #
    return(invisible(NULL))
}

# repeat_dyads_check
# 
#' Check whether dyadic observations are repeating in the data.frame object
#'
#' This function checks for repeating dyadic observations in a data.frame,
#' possibly considering a time dimension. It leverages fast C++ code for improved
#' performance.
#'
#' @param dyad_data A data.frame containing the dyadic data.
#' @param actor1 Character string specifying the column name for actor1.
#' @param actor2 Character string specifying the column name for actor2.
#' @param time Optional character string specifying the column name for time.
#'             If not provided, dyads are considered without regard to time.
#' @return An integer count of the number of repeating dyads in the data.frame.
#' @author Shahryar Minhas
#'
#' @keywords internal
#' @noRd

repeat_dyads_check <- function(dyad_data, actor1, actor2, time = NULL) {

    # clean up params
    if (!is.null(time)) {
        timeVec <- dyad_data[[time]]
    } else {
        timeVec <- rep(1, nrow(dyad_data))  # Default time vector if none provided
    }

    #
    actor1Vec <- dyad_data[[actor1]]
    actor2Vec <- dyad_data[[actor2]]

    #  
    out <- count_duplicate_dyads(actor1Vec, actor2Vec, timeVec)

    # 
    return(out)
}


#' edge_value_check
#' 
#' Warns user about how edge values in adjacency matrices will be determined
#' @param weight user inputted weight value
#' @param sum_dyads user inputted sum_dyads logical
#' @param time logical indicating whether inputted data is longitudinal
#' @return returns a NULL object but provides warnings to users
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @keywords internal
#' @noRd

edge_value_check <- function( weight, sum_dyads, time=FALSE ){

    # gen warning starter
    if(!time){ rep_wrn <- 'Warning: there are repeating dyads in the dataset. ' }
    if(time){ rep_wrn <- 'Warning: there are repeating dyads within time periods in the dataset. ' }    

    # repeating dyads with no weight
    # and sum_dyads set to false
    # example use case for us: event data where we want to produce binary edges indicating whether a pair of actors ever interacted
    if(is.null(weight) & !sum_dyads){
        cli::cli_alert_warning(
            paste0(rep_wrn, "When `weight` is not supplied and `sum_dyads` is set to FALSE, edges in the outputted adjacency matrix will represent binary interactions between actors.")) }

    # repeating dyads with weight variable not supplied and sum dyads set to true
    # example use case for us: event data where we want to produce edges indicating the total number of interactions between actors
    if(is.null(weight) & sum_dyads){
        cli::cli_alert_warning(
            paste0(rep_wrn, "When `sum_dyads = TRUE` and `weight` is not supplied, edges in the outputted adjacency matrix represent a count of interactions between actors.")) }

    # repeating dyads with weight variable supplied
    # example use case for us: event data where we want to produce edges representing the sum of weighted interactions
    if(!is.null(weight) & !sum_dyads){
        cli::cli_alert_danger(
            paste0(rep_wrn, "When `sum_dyads = FALSE` and `weight` variable is supplied but there are repeating dyads in the dataset, we cannot uniquely identify edges. Try sum_dyads=TRUE or remove repeating dyads.")) }

    #
    return(invisible(NULL))
}

#' add_var_time_check
#' 
#' Stops the process if the user tries to add time to a 
#' non-longitudinal dataset and vice versa
#' 
#' @param netlet user inputted netlet object
#' @param time user inputted time variable
#' @return returns a NULL object but stops the process
#' if there is an error detected
#' 
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @keywords internal
#' @noRd

add_var_time_check <- function( netlet, time ){

    # stop process if time variable supplied and netify type is cross-sec
    if( attributes(netlet)$netify_type == 'cross_sec' & !is.null(time) ){
        cli::cli_alert_danger('Time variable should be left to NULL, if netlet is of object type `cross_sec`.')
        stop() }

    # stop process if time variable not supplied and netify type is longit
    if( attributes(netlet)$netify_type != 'cross_sec' & is.null(time) ){
        cli::cli_alert_danger('Time variable should be supplied, if netlet is of object type `longit_array` or `longit_list`.')
        stop() }
}


#' Validate actor data for network creation
#'
#' This function validates that actor columns contain valid data and
#' performs mode-specific checks (e.g., bipartite actor distinctness)
#'
#' @param dyad_data a data frame containing dyadic data
#' @param actor1 character: name of the actor 1 variable in the data
#' @param actor2 character: name of the actor 2 variable in the data
#' @param mode character: whether the network is unipartite or bipartite
#'
#' @return invisible(TRUE) if validation passes, otherwise throws error
#' 
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @keywords internal
#' @noRd

actor_mode_check <- function(dyad_data, actor1, actor2, mode) {
    
    # Extract unique actors from each column
    actors_1 <- unique(dyad_data[,actor1])
    actors_2 <- unique(dyad_data[,actor2])
    
    # Check for empty actor sets
    if(length(actors_1) == 0) {
        cli::cli_abort(
            c("x" = "No valid actors found in actor1 column '{actor1}'.",
              "!" = "Please check your data and column specification.")
        )
    }
    
    if(length(actors_2) == 0) {
        cli::cli_abort(
            c("x" = "No valid actors found in actor2 column '{actor2}'.",
              "!" = "Please check your data and column specification.")
        )
    }
    
    # Mode-specific validation
    if(mode == 'bipartite') {
        bipartite_actor_check(actors_1, actors_2)
    } else if(mode == 'unipartite') {
        unipartite_actor_check(actors_1, actors_2)
    } else {
        cli::cli_abort(
            c("x" = "Unknown mode '{mode}'.",
              "!" = "Mode must be either 'unipartite' or 'bipartite'.")
        )
    }
    
    invisible(TRUE)
}

#' Validate bipartite network actor requirements
#'
#' @param actors_1 character vector of unique actor1 values
#' @param actors_2 character vector of unique actor2 values
#'
#' @return invisible(TRUE) if validation passes, otherwise throws error or warning
#'
#' 
#' @author Cassy Dorff, Shahryar Minhas
#' 
#' @keywords internal
#' @noRd

bipartite_actor_check <- function(actors_1, actors_2) {
    
    # Check for overlapping actors (warning, not error)
    overlap <- intersect(actors_1, actors_2)
    if(length(overlap) > 0) {
        cli::cli_alert_warning(
            c("!" = "Mode specified as 'bipartite' but {length(overlap)} actor{?s} appear{?s/} in both actor1 and actor2:",
              "i" = "Overlapping actors: {.val {head(overlap, 5)}}{if(length(overlap) > 5) ' ...'}")
        )
    }
    
    # Check for sufficient actors in each mode (error)
    if(length(actors_1) < 1 || length(actors_2) < 1) {
        cli::cli_abort(
            c("x" = "Bipartite networks require actors in both modes.",
              "i" = "Found {length(actors_1)} unique actor1 value{?s} and {length(actors_2)} unique actor2 value{?s}.")
        )
    }
    
    invisible(TRUE)
}

#' Validate unipartite network actor requirements
#'
#' @param actors_1 character vector of unique actor1 values
#' @param actors_2 character vector of unique actor2 values
#'
#' @return invisible(TRUE) if validation passes, otherwise throws error or warning
#'
#' @author Cassy Dorff, Shahryar Minhas
#' 
#' @keywords internal
#' @noRd

unipartite_actor_check <- function(actors_1, actors_2) {
    
    # For unipartite networks, we might want to check other things
    # For now, just ensure we have some actors
    all_actors <- unique(c(actors_1, actors_2))
    
    if(length(all_actors) < 4) {
        cli::cli_alert_warning(
            c("!" = "Unipartite network has only {length(all_actors)} unique actor{?s}.",
              "i" = " Networks with fewer than 4 actors may not be meaningful.")
        )
    }
    
    invisible(TRUE)
}
