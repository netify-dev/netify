#' Convert netify objects back to dyadic data frames
#'
#' `unnetify` (also available as `to_df`, `netify_to_df`, `dyadify`) reverses 
#' the netify transformation by converting network objects back into dyadic 
#' (edge-level) data frames. This function combines network structure with any 
#' associated nodal and dyadic attributes, creating a data frame where each row 
#' represents a dyad (edge) with all relevant attributes attached.
#'
#' @param netlet A netify object to be converted to dyadic format.
#' @param remove_zeros Logical. If TRUE, removes dyads with zero edge weights 
#'   from the output, resulting in a data frame of only non-zero relationships. 
#'   If FALSE (default), includes all possible dyads in the network.
#'
#' @return A data frame with one row per dyad containing:
#'   \itemize{
#'     \item \strong{from}: Name/ID of the first actor in the dyad
#'     \item \strong{to}: Name/ID of the second actor in the dyad
#'     \item \strong{time}: Time period (for longitudinal networks)
#'     \item \strong{weight column}: Edge weight values (column named after 
#'       the weight parameter used in netify)
#'     \item \strong{from_id}: Unique identifier combining actor and time (longitudinal only)
#'     \item \strong{to_id}: Unique identifier combining actor and time (longitudinal only)
#'     \item \strong{Dyadic attributes}: Any edge-level covariates from the original data
#'     \item \strong{Nodal attributes}: Actor-level covariates merged onto dyads:
#'       \itemize{
#'         \item For directed networks: suffixed with "_from" and "_to"
#'         \item For undirected networks: suffixed with "_dyad"
#'       }
#'   }
#'
#' @details
#' This function essentially reverses the netify process, making it useful for:
#' \itemize{
#'   \item Exporting network data for analysis in other software
#'   \item Creating dyadic datasets for regression analysis
#'   \item Inspecting network data in familiar data frame format
#'   \item Merging network results back with other dyadic covariates
#' }
#' 
#' For directed networks, nodal attributes are attached twice - once for the 
#' sender (suffixed "_from") and once for the receiver (suffixed "_to"). This 
#' allows for modeling sender and receiver effects separately.
#' 
#' For undirected networks, nodal attributes are attached once per dyad with 
#' the suffix "_dyad", since there is no meaningful distinction between sender 
#' and receiver.
#' 
#' The function handles both cross-sectional and longitudinal netify objects, 
#' automatically detecting the structure and adjusting the output accordingly.
#'
#' @note 
#' For large networks, setting `remove_zeros = FALSE` can result in very large 
#' data frames, as it includes all possible dyads (n × (n-1) for directed networks 
#' or n × (n-1) / 2 for undirected networks).
#'
#' @examples
#' # Load example data
#' data(icews)
#' 
#' # Create a netify object with attributes
#' icews_10 <- icews[icews$year == 2010,]
#' 
#' verbCoop_net <- netify(
#'   dyad_data = icews_10,
#'   actor1 = 'i', actor2 = 'j',
#'   symmetric = FALSE,
#'   weight = 'verbCoop',
#'   nodal_vars = c('i_polity2', 'i_log_gdp'),
#'   dyad_vars = c('verbConf', 'matlConf')
#' )
#' 
#' # Convert back to dyadic data frame
#' dyad_df <- unnetify(verbCoop_net)
#' 
#' # Examine structure
#' head(dyad_df)
#' names(dyad_df)
#' 
#' # Remove zero-weight dyads for more compact output
#' dyad_df_nonzero <- unnetify(verbCoop_net, remove_zeros = TRUE)
#' nrow(dyad_df_nonzero)  # Much smaller than full dyadic dataset
#' 
#' # Note how nodal attributes are added
#' # For directed network: _from and _to suffixes
#' head(dyad_df[, c("from", "to", "i_polity2_from", "i_polity2_to")])
#' 
#' # Longitudinal example
#' verbCoop_longit <- netify(
#'   dyad_data = icews,
#'   actor1 = 'i', actor2 = 'j', 
#'   time = 'year',
#'   symmetric = FALSE,
#'   weight = 'verbCoop',
#'   nodal_vars = c('i_polity2', 'i_log_gdp')
#' )
#' 
#' # Convert longitudinal network
#' dyad_df_longit <- unnetify(verbCoop_longit, remove_zeros = TRUE)
#' 
#' # Check time periods are included
#' table(dyad_df_longit$time)
#' 
#' # Each dyad now has associated time period
#' # Note: weight column is named after the weight variable (verbCoop)
#' head(dyad_df_longit[, c("from", "to", "time", "verbCoop")])
#' 
#' # Use the output for further analysis
#' \dontrun{
#' # For example, regression analysis
#' lm(verbCoop ~ i_polity2_from + i_polity2_to + verbConf, data = dyad_df)
#' }
#'
#' @author Cassy Dorff, Shahryar Minhas
#' 
#' @export unnetify
#' @aliases netify_to_df, to_df, dyadify
#' 

unnetify <- function(netlet, remove_zeros=FALSE){

	######################
    # check if netify object
    netify_check(netlet)  

    # get attributes
    obj_attrs = attributes(netlet)
	######################

	######################
    # get to df format
    net_dfs = decompose_netlet( netlet, remove_zeros=remove_zeros )
    edge_data = net_dfs$edge_data
    nodal_data = net_dfs$nodal_data
    rm(net_dfs)

    # create id vars
    edge_data$from_id = with(edge_data, paste(from, time, sep='_'))
    edge_data$to_id = with(edge_data, paste(to, time, sep='_'))
    nodal_data$id = with(nodal_data, paste(name, time, sep='_'))
	######################

	######################
    # merge nodal and dyad 
    nodal_vars = names(nodal_data)

    # remove id vars
    nodal_vars = setdiff(
        nodal_vars, 
        c('name', 'time', 'id') )

    # iterate through nodal_vars
    # and merge into edge_data
    for(nv in nodal_vars){

        # if not symmetric
        if(!obj_attrs$symmetric){

            # add from rel
            edge_data$tmp = nodal_data[match(edge_data$from_id, nodal_data$id),nv]
            names(edge_data)[ncol(edge_data)] = paste(nv, 'from', sep="_")

            # add to rel
            edge_data$tmp = nodal_data[match(edge_data$to_id, nodal_data$id),nv]
            names(edge_data)[ncol(edge_data)] = paste(nv, 'to', sep="_")
        }

        # if symmetric
        if(obj_attrs$symmetric){

            # add from rel
            edge_data$tmp = nodal_data[match(edge_data$from_id, nodal_data$id),nv]
            names(edge_data)[ncol(edge_data)] = paste(nv, 'dyad', sep="_")
        }
    }
	######################

	######################
    #
    return(edge_data) 
	######################        
}

#' @rdname unnetify
#' @export
to_df <- unnetify

#' @rdname unnetify
#' @export
netify_to_df <- unnetify

#' @rdname unnetify
#' @export
dyadify <- unnetify