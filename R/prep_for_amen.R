#' Convert netify objects to amen package format
#'
#' `prep_for_amen` (also available as `netify_to_amen`, `to_amen`) 
#' transforms netify network objects into the specific data 
#' structure required by the `amen` package for network analysis and modeling. 
#' The amen package implements various network models including the Social 
#' Relations Model (SRM) and Additive and Multiplicative Effects (AME) models.
#'
#' @param netlet A netify object containing network data. Currently supports 
#'   single-layer networks only. For multilayer networks, use 
#'   \code{\link{subset_netlet}} to extract individual layers first.
#'
#' @return A list with components formatted for amen analysis:
#'   \item{Y}{The network adjacency matrix or array. For cross-sectional data, 
#'     this is a matrix. For longitudinal data, this is a 3-dimensional array 
#'     with dimensions: actors by actors by time.}
#'   \item{Xdyad}{Array of dyadic covariates with dimensions: row by column by covariate 
#'     for cross-sectional data, or row by column by covariate by time for 
#'     longitudinal data. NULL if no dyadic variables exist.}
#'   \item{Xrow}{Matrix or array of row actor (sender) covariates. For cross-sectional 
#'     data: row actors by covariates. For longitudinal: row actors by covariates by time. 
#'     NULL if no nodal variables exist.}
#'   \item{Xcol}{Matrix or array of column actor (receiver) covariates. Same structure 
#'     as Xrow. For symmetric networks, Xcol will be identical to Xrow.}
#'
#' @details
#' The function handles three types of netify objects:
#' \itemize{
#'   \item **Cross-sectional networks**: Produces matrices for Y, Xrow, and Xcol, 
#'     and a 3D array for Xdyad
#'   \item **Longitudinal arrays**: Maintains array structure, adding time dimension 
#'     where needed
#'   \item **Longitudinal lists**: Converts list format to arrays suitable for amen
#' }
#' 
#' All nodal attributes must be numeric for compatibility with amen models. The 
#' function will stop with an error if non-numeric nodal variables are detected.
#' 
#' Missing values in the network or covariates are preserved as NA, allowing 
#' amen's modeling functions to handle them appropriately.
#'
#'
#' @examples
#' # Load example data
#' data(icews)
#' 
#' # Cross-sectional example
#' icews_10 <- icews[icews$year == 2010,]
#' 
#' # Create netify object with nodal and dyadic attributes
#' icews_net <- netify(
#'   dyad_data = icews_10, 
#'   actor1 = 'i', actor2 = 'j', 
#'   symmetric = FALSE, weight = 'matlConf',
#'   nodal_vars = c('i_polity2', 'i_log_gdp', 'i_log_pop'),
#'   dyad_vars = c('matlCoop', 'verbCoop', 'verbConf'),
#'   dyad_vars_symmetric = c(FALSE, FALSE, FALSE)
#' )
#' 
#' # Convert to amen format
#' amen_data <- prep_for_amen(icews_net)
#' 
#' # Examine structure
#' str(amen_data)
#' 
#' # Y contains the network
#' dim(amen_data$Y)  # actors x actors
#' 
#' # Xdyad contains dyadic covariates
#' dim(amen_data$Xdyad)  # actors x actors x covariates
#' 
#' # Xrow and Xcol contain actor attributes
#' dim(amen_data$Xrow)  # actors x attributes
#' 
#' # Longitudinal example
#' icews_longit <- netify(
#'   dyad_data = icews,
#'   actor1 = 'i', actor2 = 'j', time = 'year',
#'   symmetric = FALSE, weight = 'matlConf',
#'   nodal_vars = c('i_polity2', 'i_log_gdp', 'i_log_pop'),
#'   dyad_vars = c('matlCoop', 'verbCoop', 'verbConf'),
#'   dyad_vars_symmetric = c(FALSE, FALSE, FALSE)
#' )
#' 
#' # Convert longitudinal data
#' amen_longit <- prep_for_amen(icews_longit)
#' 
#' # Now arrays have time dimension
#' dim(amen_longit$Y)      # actors x actors x time
#' dim(amen_longit$Xdyad)  # actors x actors x covariates x time
#'
#' @author Ha Eun Choi, Cassy Dorff, Colin Henry, Shahryar Minhas
#'
#' @export prep_for_amen
#' @aliases netify_to_amen, to_amen, amen_ify

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

    if( dyad_data_exists ){

      # create dyad array
      var_matrices <- attr(netlet, 'dyad_data')[[1]]
      first_matrix <- var_matrices[[1]]
      dyad_arr <- array(
        data = unlist(var_matrices, use.names = FALSE),
        dim = c(
          nrow(first_matrix), ncol(first_matrix), 
          length(var_matrices)),
        dimnames = list(
          rownames(first_matrix), colnames(first_matrix), 
          names(var_matrices)) )
    } else { dyad_arr <- NULL }

    # cross-sec output
    out <- list(
      Y=get_raw(netlet),
      Xdyad=dyad_arr,
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

#' @rdname prep_for_amen
#' @export
netify_to_amen <- prep_for_amen

#' @rdname prep_for_amen
#' @export
to_amen <- prep_for_amen

#' @rdname prep_for_amen
#' @export
amen_ify <- prep_for_amen