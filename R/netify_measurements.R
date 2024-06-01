#' Get measurements of a netify object
#' 
#' @param netlet a netify object
#' @return a list of measurements about the netify object
#' @author Cassy Dorff, Shahryar Minhas
#' @export netify_measurements

netify_measurements <- function(netlet){

  # check if netify object
  netify_check(netlet)

  ## three cases: cross-sec/matrix, longit list, longit array
  netlet_type = attributes(netlet)$netify_type

  # check other attributes
  nodal_data_exist = !is.null( 
    attributes(netlet)$nodal_data[[1]] )
  dyad_data_exist = !is.null( 
    attributes(netlet)$dyad_data[[1]] )
  layers_exist = ifelse(
    length(attributes(netlet)$layers)>1, TRUE, FALSE)

  # output object
  msrmnts <- list(
    row_actors=NULL, col_actors=NULL, time = NULL, layers=NULL,
    n_row_actors=NULL, n_col_actors=NULL, n_time = NULL, n_layers=NULL,
    nvars=NULL, n_nvars=NULL, dvars=NULL, n_dvars=NULL )

  # cross-sec
  if(netlet_type == 'cross_sec'){
    
    # dv measurements
    msrmnts$row_actors <- rownames(netlet)
    msrmnts$col_actors <- colnames(netlet)
    msrmnts$n_row_actors <- nrow(netlet)
    msrmnts$n_col_actors <- ncol(netlet)

    # add layer info if more than one
    if(layers_exist){
      msrmnts$layers <- attributes(netlet)$layers
      msrmnts$n_layers <- length(attributes(netlet)$layers) 
    } else {
      msrmnts$layers <- NULL
      msrmnts$n_layers <- NULL
    }

    # check nodal data
    if(nodal_data_exist){
      msrmnts$nvars <- colnames(attributes(netlet)$nodal_data)[-1]
      msrmnts$n_nvars <- ncol(attributes(netlet)$nodal_data) - 1 }

    # check dyad data
    if(dyad_data_exist){
      msrmnts$dvars <- dimnames(attributes(netlet)$dyad_data[[1]])[[3]]
      msrmnts$n_dvars <- dim(attributes(netlet)$dyad_data[[1]])[3] }
  }

  # longit array
  if(netlet_type == 'longit_array'){

    # dv measurements
    msrmnts$row_actors <- rownames(netlet)
    msrmnts$col_actors <- colnames(netlet)
    msrmnts$n_row_actors <- nrow(netlet)
    msrmnts$n_col_actors <- ncol(netlet)

    # add layer info if more than one
    if(layers_exist){
      msrmnts$layers <- attributes(netlet)$layers
      msrmnts$n_layers <- length(attributes(netlet)$layers)
      # time gets shifted to fourth dim if layers exist
      msrmnts$time <- dimnames(netlet)[[4]]
      msrmnts$n_time <- dim(netlet)[4]
    } else {
      msrmnts$layers <- NULL
      msrmnts$n_layers <- NULL
      # time gets shifted to third  dim if layers exist
      msrmnts$time <- dimnames(netlet)[[3]]      
      msrmnts$n_time <- dim(netlet)[3]
    }

    # check nodal data
    if(nodal_data_exist){
      msrmnts$nvars <- colnames(attributes(netlet)$nodal_data)[-c(1:2)]
      msrmnts$n_nvars <- ncol(attributes(netlet)$nodal_data) - 2 }

    # check dyad data
    if(dyad_data_exist){
      msrmnts$dvars <- dimnames(attributes(netlet)$dyad_data[[1]])[[3]]
      msrmnts$n_dvars <- dim(attributes(netlet)$dyad_data[[1]])[3] }    
  }

  # longit list
  if(netlet_type == 'longit_list'){

    # dv measurements
    msrmnts$row_actors <- lapply(netlet, rownames)
    msrmnts$col_actors <- lapply(netlet, colnames)
    msrmnts$time <- names(netlet)
    msrmnts$n_row_actors <- lapply(netlet, nrow)
    msrmnts$n_col_actors <- lapply(netlet, ncol)
    msrmnts$n_time <- length(netlet)

    # add layer info if more than one
    if(layers_exist){
      msrmnts$layers <- attributes(netlet)$layers
      msrmnts$n_layers <- length(attributes(netlet)$layers) 
    } else {
      msrmnts$layers <- NULL
      msrmnts$n_layers <- NULL
    }

    # check nodal data
    if(nodal_data_exist){
      msrmnts$nvars <- colnames(attributes(netlet)$nodal_data)[-c(1:2)]
      msrmnts$n_nvars <- ncol(attributes(netlet)$nodal_data) - 2 }

    # check dyad data
    if(dyad_data_exist){
      msrmnts$dvars <- dimnames(attributes(netlet)$dyad_data[[1]])[[3]]
      msrmnts$n_dvars <- dim(attributes(netlet)$dyad_data[[1]])[3] }
  }

  # 
  return(msrmnts)
}