#' Convert netify object to amen structured input
#'
#' @aliases prep_for_amen
#' @param netlet An R object
#' @return object ready for analysis with amen
#' @author Ha Eun Choi, Cassy Dorff, Colin Henry, Shahryar Minhas
#'
#' @keywords netify
#' @examples
#'
#' # load icews data
#' data(icews)
#' 
#' # filter to a year for cross-sec example
#' icews_10 <- icews[icews$year == 2010,]
#' 
#' # netify object
#' icews_matlConf <- netify(
#'   dyad_data = icews_10, 
#'   actor1 = 'i', actor2 = 'j', 
#'   symmetric = FALSE, weight = 'matlConf',
#'   nodal_vars = c('i_polity2', 'i_log_gdp', 'i_log_pop'),
#'   dyad_vars = c('matlCoop', 'verbCoop', 'verbConf'),
#'   dyad_vars_symmetric = c(FALSE, FALSE, FALSE) )
#' 
#' # convert to amen input
#' for_amen <- prep_for_amen(icews_matlConf)
#' 
#' # for_amen$Y is the matrix of dyadic weights
#' dim(for_amen$Y)
#' 
#' # for_amen$Xdyad is the array of dyadic attributes
#' dim(for_amen$Xdyad)
#' 
#' # for_amen$Xrow is the matrix of nodal attributes for rows
#' dim(for_amen$Xrow)
#' 
#' # for_amen$Xcol is the matrix of nodal attributes for columns
#' dim(for_amen$Xcol)
#' 
#' # generate a longitudional, directed and weighted network
#' # where the weights are matlConf and results are organized
#' # in an array and we have both dyadic and nodal attributes
#' icews_matlConf_longit <- netify(
#'   dyad_data=icews,
#'   actor1='i', actor2='j', time='year',
#'   symmetric=FALSE, weight='matlConf',
#'   nodal_vars=c('i_polity2', 'i_log_gdp', 'i_log_pop'),
#'   dyad_vars=c('matlCoop', 'verbCoop', 'verbConf'),
#'   dyad_vars_symmetric=c(FALSE, FALSE, FALSE) )
#' 
#' # convert to amen input
#' for_amen_longit <- prep_for_amen(icews_matlConf_longit)
#' 
#' # for_amen_longit$Y is the array of dyadic weights
#' dim(for_amen_longit$Y)
#' 
#' # for_amen_longit$Xdyad is the array of dyadic attributes
#' dim(for_amen_longit$Xdyad)
#' 
#' # for_amen_longit$Xrow is the array of nodal attributes for rows
#' dim(for_amen_longit$Xrow)
#' 
#' # for_amen_longit$Xcol is the array of nodal attributes for columns
#' dim(for_amen_longit$Xcol)
#' 
#' @export prep_for_amen

prep_for_amen <- function(netlet){

  # check if netify object
  netify_check(netlet)

# if more than one layer tell user they must specify a single layer
	if(length(attributes(netlet)$layers) > 1){
		cli::cli_alert_danger(
			'Error: This object has multiple layers. 
      `prep_for_amen` does not currently support multilayer `netify` inputs. 
      Please use the `subset_netlet` function to create a `netify` object with a single layer.' )
		stop() }

  # check attributes
  nodal_data_exists <- !is.null(attr(netlet, 'nodal_data'))
  dyad_data_exists <- !is.null(attr(netlet, 'dyad_data'))

  # get dimensions
  msrmnts <- netify_measurements(netlet)

  # make sure nodal attributes are numeric
  if( nodal_data_exists ){
    nvar_class_check <- apply(
      attr(netlet, 'nodal_data')[,msrmnts$nvars], 
      2, is.numeric )
    if( !all( nvar_class_check ) ){
      cli::cli_alert_danger(
        'Error: All nodal attributes must be numeric.' )
      stop() } }

  ## three cases: cross-sec/matrix, longit list, longit array
  netlet_type = attributes(netlet)$netify_type

  # cross-sec case
  if(netlet_type == 'cross_sec'){

    # if present, convert nodal_data attribute into a 
    # data.matrix with rownames
    if( nodal_data_exists ){

      # organize nodal data separately for rows and cols
      n_list <- lapply(c('row', 'col'), function(dlab){

        # set up matrix to fill in for particular dim
        mat_dim <- c(
          msrmnts[[paste0('n_', dlab, '_actors')]],
          msrmnts$n_nvars )
        mat_labs <- list(
          msrmnts[[paste0(dlab, '_actors')]],
          msrmnts$nvars )
        mat <- array(NA, mat_dim, dimnames=mat_labs)

        # fill in matrix
        to_add <- attr(netlet, 'nodal_data')
        to_add_rows <- to_add$actor
        to_add <- to_add[,msrmnts$nvars,drop=FALSE]
        to_add <- data.matrix(to_add)
        rownames(to_add) <- to_add_rows
        mat[to_add_rows,] = to_add
        return(mat) }) 
      } else { n_list <- list(NULL,NULL) }
      names(n_list) = c('row', 'col')

      out <- list(
        Y=get_raw(netlet),
        Xdyad=attr(netlet, 'dyad_data')[[1]],
        Xrow=n_list$'row',
        Xcol=n_list$'col' ) }

  # array case
  if(netlet_type == 'longit_array'){
    out <- list(
      Y=get_raw(netlet),
      Xdyad=longit_dyad_to_arr(netlet),
      Xrow=longit_nodal_to_arr(netlet)$'row',
      Xcol=longit_nodal_to_arr(netlet)$'col' ) }

  # list case
  if(netlet_type == 'longit_list'){
    out <- list(
      Y=longit_dv_to_arr(netlet),
      Xdyad=longit_dyad_to_arr(netlet),
      Xrow=longit_nodal_to_arr(netlet)$'row',
      Xcol=longit_nodal_to_arr(netlet)$'col' ) }  

  # 
  return(out)
}