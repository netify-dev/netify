#' Convert netify objects back to dyadic data frames
#'
#' `unnetify` (also available as `netify_to_df`) reverses
#' the netify transformation by converting network objects back into dyadic
#' (edge-level) data frames. this function combines network structure with any
#' associated nodal and dyadic attributes, creating a data frame where each row
#' represents a dyad (edge) with all relevant attributes attached.
#'
#' @param netlet a netify object to be converted to dyadic format.
#' @param remove_zeros logical. if TRUE, removes dyads with zero edge weights
#'   from the output, resulting in a data frame of only non-zero relationships.
#'   if FALSE (default), includes all possible dyads in the network.
#'
#' @return a data frame with one row per dyad containing:
#'   \itemize{
#'     \item \strong{from}: name/id of the first actor in the dyad
#'     \item \strong{to}: name/id of the second actor in the dyad
#'     \item \strong{time}: time period (for longitudinal networks)
#'     \item \strong{weight column}: edge weight values (column named after
#'       the weight parameter used in netify)
#'     \item \strong{from_id}: unique identifier combining actor and time (longitudinal only)
#'     \item \strong{to_id}: unique identifier combining actor and time (longitudinal only)
#'     \item \strong{dyadic attributes}: any edge-level covariates from the original data
#'     \item \strong{nodal attributes}: actor-level covariates merged onto dyads,
#'       suffixed with "_from" and "_to" to match the corresponding actor
#'   }
#'
#' @details
#' this function essentially reverses the netify process, making it useful for:
#' \itemize{
#'   \item exporting network data for analysis in other software
#'   \item creating dyadic datasets for regression analysis
#'   \item inspecting network data in familiar data frame format
#'   \item merging network results back with other dyadic covariates
#' }
#'
#' nodal attributes are attached twice per dyad - once for the "from" actor
#' (suffixed "_from") and once for the "to" actor (suffixed "_to"). this
#' applies to both directed and undirected networks, ensuring that both
#' actors' attributes are available for dyadic analysis.
#'
#' the function handles both cross-sectional and longitudinal netify objects,
#' automatically detecting the structure and adjusting the output accordingly.
#'
#' @note
#' for large networks, setting `remove_zeros = FALSE` can result in very large
#' data frames, as it includes all possible dyads (n x (n-1) for directed networks
#' or n x (n-1) / 2 for undirected networks).
#'
#' @examples
#' # load example data
#' data(icews)
#'
#' # create a netify object with attributes
#' icews_10 <- icews[icews$year == 2010, ]
#'
#' verbCoop_net <- netify(
#'     icews_10,
#'     actor1 = "i", actor2 = "j",
#'     symmetric = FALSE,
#'     weight = "verbCoop",
#'     nodal_vars = c("i_polity2", "i_log_gdp"),
#'     dyad_vars = c("verbConf", "matlConf")
#' )
#'
#' # convert back to dyadic data frame
#' dyad_df <- unnetify(verbCoop_net)
#'
#' # examine structure
#' head(dyad_df)
#' names(dyad_df)
#'
#' # remove zero-weight dyads for more compact output
#' dyad_df_nonzero <- unnetify(verbCoop_net, remove_zeros = TRUE)
#' nrow(dyad_df_nonzero) # much smaller than full dyadic dataset
#'
#' # note how nodal attributes are added
#' # for directed network: _from and _to suffixes
#' head(dyad_df[, c("from", "to", "i_polity2_from", "i_polity2_to")])
#'
#' # longitudinal example
#' verbCoop_longit <- netify(
#'     icews,
#'     actor1 = "i", actor2 = "j",
#'     time = "year",
#'     symmetric = FALSE,
#'     weight = "verbCoop",
#'     nodal_vars = c("i_polity2", "i_log_gdp")
#' )
#'
#' # convert longitudinal network
#' dyad_df_longit <- unnetify(verbCoop_longit, remove_zeros = TRUE)
#'
#' # check time periods are included
#' table(dyad_df_longit$time)
#'
#' # each dyad now has associated time period
#' # note: weight column is named after the weight variable (verbCoop)
#' head(dyad_df_longit[, c("from", "to", "time", "verbCoop")])
#'
#' # use the output for further analysis
#' \donttest{
#' # for example, regression analysis
#' lm(verbCoop ~ i_polity2_from + i_polity2_to + verbConf, data = dyad_df)
#' }
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export unnetify
#' @aliases netify_to_df
#'

unnetify <- function(netlet, remove_zeros = FALSE) {
	####
	# check if netify object
	netify_check(netlet)

	# get attributes
	obj_attrs <- attributes(netlet)
	####

	####
	# get to df format
	net_dfs <- decompose_netify(netlet, remove_zeros = remove_zeros)
	edge_data <- net_dfs$edge_data
	nodal_data <- net_dfs$nodal_data
	rm(net_dfs)

	# create id vars
	edge_data$from_id <- with(edge_data, paste(from, time, sep = "_"))
	edge_data$to_id <- with(edge_data, paste(to, time, sep = "_"))
	nodal_data$id <- with(nodal_data, paste(name, time, sep = "_"))
	####

	####
	# merge nodal and dyad
	nodal_vars <- names(nodal_data)

	# remove id vars
	nodal_vars <- setdiff(
		nodal_vars,
		c("name", "time", "id")
	)

	# iterate through nodal_vars
	# and merge into edge_data
	for (nv in nodal_vars) {
		# add from rel
		edge_data$tmp <- nodal_data[match(edge_data$from_id, nodal_data$id), nv]
		names(edge_data)[ncol(edge_data)] <- paste(nv, "from", sep = "_")

		# add to rel
		edge_data$tmp <- nodal_data[match(edge_data$to_id, nodal_data$id), nv]
		names(edge_data)[ncol(edge_data)] <- paste(nv, "to", sep = "_")
	}
	####

	####
	#
	return(edge_data)
	####
}

#' @rdname unnetify
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export
netify_to_df <- unnetify
