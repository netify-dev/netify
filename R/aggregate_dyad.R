#' Aggregate dyadic event data by actor pairs
#'
#' `aggregate_dyad` is designed for use with
#' dyadic event datasets such as those from acled or icews. these datasets often contain
#' multiple interactions between the same pair of actors--e.g., protest events, material
#' cooperation, or verbal conflict--recorded at high frequency. this function aggregates
#' such repeated observations into a single summary value per dyad, optionally within
#' specified time periods. it is particularly useful for preparing network inputs by
#' collapsing daily or monthly event-level data into actor-to-actor matrices.
#'
#' @param dyad_data a data.frame containing dyadic observations. must include
#'   columns for two actors and a weight variable. will be coerced to data.frame
#'   if a tibble or data.table is provided.
#' @param actor1 character string specifying the column name for the first actor
#'   in each dyad.
#' @param actor2 character string specifying the column name for the second actor
#'   in each dyad.
#' @param time character string specifying the column name for time periods.
#'   if NULL (default), aggregation is performed across all time periods.
#' @param weight character string specifying the column name containing values
#'   to be aggregated (summed) for each unique actor pair.
#' @param symmetric logical. if TRUE, treats dyads as undirected (i.e., the dyad
#'   a-b is treated as identical to b-a). if FALSE, treats dyads as directed
#'   (i.e., a-b is distinct from b-a).
#' @param ignore_missing logical. if TRUE (default), missing values in the weight
#'   variable are ignored during aggregation. if FALSE, any dyad containing a
#'   missing value will result in na for that aggregated dyad.
#'
#' @return a data.frame with unique actor pairs (and time periods if specified)
#'   and their aggregated weight values. the output contains columns:
#'   \itemize{
#'     \item \strong{actor1}: first actor in each dyad (using original column name)
#'     \item \strong{actor2}: second actor in each dyad (using original column name)
#'     \item \strong{time}: time period if time parameter was specified (using original column name)
#'     \item \strong{weight}: aggregated (summed) values for each unique dyad (using original column name)
#'   }
#'
#' @details
#' the function handles both directed and undirected dyadic aggregation:
#'
#' \strong{for symmetric (undirected) networks:}
#'
#' the function uses an efficient aggregation method that:
#' \enumerate{
#'   \item creates symmetric identifiers for each dyad using `gen_symm_id` (where a-b = b-a)
#'   \item aggregates values by these symmetric identifiers
#'   \item expands the results back to directed format for consistency with other netify functions
#' }
#'
#' this ensures that interactions between actors a and b are combined regardless
#' of direction, useful for undirected relationships like friendships or alliances.
#'
#' \strong{for asymmetric (directed) networks:}
#'
#' standard aggregation is performed treating each directed dyad separately. this
#' maintains the distinction between a->b and b->a relationships, which is important
#' for directed interactions like exports/imports or sender/receiver communications.
#'
#' \strong{missing value handling:}
#'
#' the `ignore_missing` parameter controls how na values are treated:
#' \itemize{
#'   \item when TRUE: missing values are excluded from the sum (e.g., sum(10, na, 20) = 30)
#'   \item when FALSE: any missing value results in na for that dyad (e.g., sum(10, na, 20) = na)
#' }
#'
#' @note
#' the function preserves the original column names from the input data.frame in
#' the output, making it easy to chain operations or merge results.
#'
#' when symmetric = TRUE, the output still maintains a directed format (separate
#' rows for a-b and b-a) with identical values for both directions. this ensures
#' compatibility with other netify functions that expect directed dyadic data.
#'
#'
#' @examples
#' # load example data
#' data(icews)
#'
#' # example 1: aggregate multiple events between countries
#' # the icews data contains multiple events per country pair
#' icews_2010 <- icews[icews$year == 2010, ]
#'
#' # aggregate directed cooperation events
#' agg_coop <- aggregate_dyad(
#'     dyad_data = icews_2010,
#'     actor1 = "i",
#'     actor2 = "j",
#'     weight = "verbCoop",
#'     symmetric = FALSE
#' )
#'
#' # check reduction in observations
#' nrow(icews_2010) # original observations
#' nrow(agg_coop) # unique directed dyads
#'
#' # example 2: create symmetric trade volumes
#' trade_data <- data.frame(
#'     exporter = c("usa", "usa", "china", "china", "usa", "china"),
#'     importer = c("china", "china", "usa", "usa", "uk", "uk"),
#'     year = c(2020, 2020, 2020, 2021, 2021, 2021),
#'     trade_value = c(100, 50, 75, 80, 120, 90)
#' )
#'
#' # aggregate as total trade between countries (undirected)
#' total_trade <- aggregate_dyad(
#'     dyad_data = trade_data,
#'     actor1 = "exporter",
#'     actor2 = "importer",
#'     time = "year",
#'     weight = "trade_value",
#'     symmetric = TRUE
#' )
#'
#' # usa-china trade in 2020: 100+50+75 = 225 (appears in both directions)
#' total_trade[total_trade$year == 2020, ]
#'
#' # example 3: aggregate across all time periods
#' all_time_trade <- aggregate_dyad(
#'     dyad_data = trade_data,
#'     actor1 = "exporter",
#'     actor2 = "importer",
#'     time = NULL, # aggregate across all years
#'     weight = "trade_value",
#'     symmetric = FALSE
#' )
#'
#' # usa total exports to china: 100+50 = 150
#' all_time_trade
#'
#' # example 4: handle missing values
#' trade_data_na <- trade_data
#' trade_data_na$trade_value[2] <- NA
#'
#' # ignore missing values (default)
#' agg_ignore_na <- aggregate_dyad(
#'     dyad_data = trade_data_na,
#'     actor1 = "exporter",
#'     actor2 = "importer",
#'     time = "year",
#'     weight = "trade_value",
#'     symmetric = FALSE,
#'     ignore_missing = TRUE
#' )
#'
#' # include missing values
#' agg_with_na <- aggregate_dyad(
#'     dyad_data = trade_data_na,
#'     actor1 = "exporter",
#'     actor2 = "importer",
#'     time = "year",
#'     weight = "trade_value",
#'     symmetric = FALSE,
#'     ignore_missing = FALSE
#' )
#'
#' # compare results for usa->china in 2020
#' agg_ignore_na[agg_ignore_na$exporter == "usa" &
#'     agg_ignore_na$importer == "china" &
#'     agg_ignore_na$year == 2020, ] # 100 (ignored NA)
#'
#' agg_with_na[agg_with_na$exporter == "usa" &
#'     agg_with_na$importer == "china" &
#'     agg_with_na$year == 2020, ] # NA
#'
#' @author shahryar minhas
#'
#' @importFrom stats as.formula na.pass
#'
#' @export aggregate_dyad

aggregate_dyad <- function(
	dyad_data,
	actor1,
	actor2,
	time = NULL,
	weight,
	symmetric,
	ignore_missing = TRUE) {
	# symmetric aggregation by explicit columns; do not parse compound ids
	if (symmetric) {
		a1 <- as.character(dyad_data[[actor1]])
		a2 <- as.character(dyad_data[[actor2]])
		symm_data <- data.frame(
			.actor_min = pmin(a1, a2),
			.actor_max = pmax(a1, a2),
			.weight = dyad_data[[weight]],
			stringsAsFactors = FALSE
		)
		if (!is.null(time)) {
			symm_data$.time <- dyad_data[[time]]
		}

			# sum handler honoring ignore_missing
			sum_func <- if (ignore_missing) {
				function(x, ...) sum(x, na.rm = TRUE)
			} else {
				function(x, ...) {
					if (any(is.na(x))) NA_real_ else sum(x, na.rm = FALSE)
				}
			}

		agg_result <- aggregate(
			symm_data[".weight"],
			by = symm_data[if (is.null(time)) c(".actor_min", ".actor_max") else c(".actor_min", ".actor_max", ".time")],
			FUN = sum_func,
			na.action = na.pass
		)

		# expand back to directed format
		dyad_data <- expand_symmetric_dyads(
			agg_result,
			actor1,
			actor2,
			time,
			weight
		)
	} else {
		# directed aggregation
		if (is.null(time)) {
			formula_agg <- as.formula(paste(weight, "~", actor1, "+", actor2))
		} else {
			formula_agg <- as.formula(paste(weight, "~", actor1, "+", actor2, "+", time))
		}

			sum_func <- if (ignore_missing) {
				function(x, ...) sum(x, na.rm = TRUE)
			} else {
				function(x, ...) {
					if (any(is.na(x))) NA_real_ else sum(x, na.rm = FALSE)
				}
			}

		dyad_data <- aggregate(
			formula_agg,
			data = dyad_data,
			FUN = sum_func,
			na.action = na.pass
		)

		# enforce column order
		if (is.null(time)) {
			dyad_data <- dyad_data[, c(actor1, actor2, weight)]
		} else {
			dyad_data <- dyad_data[, c(actor1, actor2, time, weight)]
		}
	}

	rownames(dyad_data) <- NULL

	return(dyad_data)
}

#' generate symmetric identifiers for dyadic data
#'
#' `gen_symm_id` creates symmetric identifiers for dyadic data, ensuring that
#' each unique pair of actors receives the same id regardless of order. this is
#' particularly useful for undirected network data where the relationship between
#' actors a and b is identical to the relationship between b and a.
#'
#' @param dyad_data a data.frame containing dyadic data with at least two columns
#'   representing actors in each dyad. will be coerced to data.frame if a tibble
#'   or data.table is provided.
#' @param actor1 character string specifying the column name for the first actor
#'   in each dyad.
#' @param actor2 character string specifying the column name for the second actor
#'   in each dyad.
#' @param time character string specifying the column name for time periods. if
#'   provided, the time value will be appended to the symmetric id to create
#'   unique identifiers for each time period. set to NULL (default) for
#'   cross-sectional data.
#'
#' @return a character vector of symmetric identifiers with the same length as
#'   the number of rows in dyad_data. each id is formatted as:
#'   \itemize{
#'     \item \strong{without time}: a length-prefixed key for the alphabetically sorted actor pair
#'     \item \strong{with time}: the same key with the time value appended
#'   }
#'
#' @details
#' the function ensures symmetry by alphabetically sorting actor names before
#' creating the identifier. this guarantees that:
#' \itemize{
#'   \item the dyad "usa-china" receives the same id as "china-usa"
#'   \item the dyad "brazil-argentina" receives the same id as "argentina-brazil"
#'   \item actor pairs are consistently ordered regardless of input order
#' }
#'
#' when a time column is specified, it's appended to the symmetric id to maintain
#' unique identifiers across different time periods. this allows for proper
#' aggregation of longitudinal dyadic data while preserving temporal variation.
#'
#'
#' @note
#' all actor values are converted to character strings before creating ids to
#' ensure consistent sorting behavior across different data types.
#'
#' ids are intended as opaque keys. do not parse them to recover actor names;
#' keep the original actor columns when those values are needed downstream.
#'
#' this function is primarily used internally by `aggregate_dyad` for efficient
#' symmetric aggregation, but can be used independently for creating symmetric
#' dyad identifiers.
#'
#' @examples
#' # create example dyadic data
#' trade_df <- data.frame(
#'     from = c("usa", "china", "russia", "usa", "brazil", "argentina"),
#'     to = c("china", "usa", "usa", "russia", "argentina", "brazil"),
#'     trade_value = c(100, 100, 50, 75, 30, 25),
#'     year = c(2020, 2020, 2021, 2021, 2021, 2021)
#' )
#'
#' # generate symmetric ids without time
#' trade_df$symm_id <- gen_symm_id(trade_df, "from", "to")
#' print(trade_df[, c("from", "to", "symm_id")])
#' # note: usa-china and china-usa both get "china_usa"
#'
#' # generate symmetric ids with time
#' trade_df$symm_id_time <- gen_symm_id(trade_df, "from", "to", "year")
#' print(trade_df[, c("from", "to", "year", "symm_id_time")])
#' # note: usa-china in 2020 gets "china_usa_2020"
#'
#' # use for aggregation of undirected relationships
#' trade_df$total_trade <- ave(
#'     trade_df$trade_value,
#'     trade_df$symm_id_time,
#'     FUN = sum
#' )
#' print(unique(trade_df[, c("symm_id_time", "total_trade")]))
#'
#' # example with longitudinal data
#' library(netify)
#' data(icews)
#' icews_sample <- icews[1:100, ]
#'
#' # create symmetric ids for conflict events
#' icews_sample$symm_dyad <- gen_symm_id(
#'     icews_sample,
#'     actor1 = "i",
#'     actor2 = "j",
#'     time = "year"
#' )
#'
#' # check that symmetric pairs get same id
#' icews_sample[icews_sample$i == "united states" & icews_sample$j == "israel", "symm_dyad"]
#' icews_sample[icews_sample$i == "israel" & icews_sample$j == "united states", "symm_dyad"]
#'
#' @author shahryar minhas
#'
#' @export gen_symm_id

gen_symm_id <- function(
	dyad_data,
	actor1,
	actor2,
	time = NULL) {
	# input validation
	if (!is.data.frame(dyad_data)) {
		cli::cli_abort(c(
			"x" = "{.arg dyad_data} must be a data.frame.",
			"i" = "You provided an object of class {.cls {class(dyad_data)}}."
		))
	}

	if (!actor1 %in% names(dyad_data)) {
		cli::cli_abort(c(
			"x" = "Column {.field {actor1}} not found in {.arg dyad_data}.",
			"i" = "Available columns: {.field {names(dyad_data)}}"
		))
	}

	if (!actor2 %in% names(dyad_data)) {
		cli::cli_abort(c(
			"x" = "Column {.field {actor2}} not found in {.arg dyad_data}.",
			"i" = "Available columns: {.field {names(dyad_data)}}"
		))
	}

	if (!is.null(time) && !time %in% names(dyad_data)) {
		cli::cli_abort(c(
			"x" = "Column {.field {time}} not found in {.arg dyad_data}.",
			"i" = "Available columns: {.field {names(dyad_data)}}"
		))
	}

	a1 <- as.character(dyad_data[[actor1]])
	a2 <- as.character(dyad_data[[actor2]])

	# pmin/pmax sorts each actor pair element-wise without an apply loop
	actor_min <- pmin(a1, a2)
	actor_max <- pmax(a1, a2)

	encode_key_field <- function(x) {
		x <- as.character(x)
		x[is.na(x)] <- "<NA>"
		paste0(nchar(x, type = "bytes"), ":", x)
	}

	symm_id <- paste(encode_key_field(actor_min), encode_key_field(actor_max), sep = "|")

	# append time if specified
	if (!is.null(time)) {
		time_vals <- as.character(dyad_data[[time]])
		symm_id <- paste(symm_id, encode_key_field(time_vals), sep = "|")
	}

	return(symm_id)
}


#' expand symmetric dyads back to directed format
#'
#' this internal function takes aggregated symmetric dyads and expands them
#' back to directed format (both a->b and b->a directions).
#'
#' @param agg_result aggregated data with .actor_min, .actor_max, and optional .time columns
#' @param actor1 name for actor1 column
#' @param actor2 name for actor2 column
#' @param time name for time column (NULL if cross-sectional)
#' @param weight name for weight column
#'
#' @return data frame with expanded dyads
#'
#' @author shahryar minhas
#'
#' @keywords internal
#' @noRd

expand_symmetric_dyads <- function(agg_result, actor1, actor2, time, weight) {
	a1_vals <- agg_result$.actor_min
	a2_vals <- agg_result$.actor_max
	wt_vals <- agg_result$.weight

	# identify self-loops
	is_self_loop <- a1_vals == a2_vals
	n_regular <- sum(!is_self_loop)
	n_self <- sum(is_self_loop)

	total_rows <- 2 * n_regular + n_self

	# build result vectors
	if (is.null(time)) {
		# cross-sectional
		result <- data.frame(
			actor1 = c(
				a1_vals[!is_self_loop],
				a2_vals[!is_self_loop],
				a1_vals[is_self_loop]
			),
			actor2 = c(
				a2_vals[!is_self_loop],
				a1_vals[!is_self_loop],
				a2_vals[is_self_loop]
			),
			weight = c(
				wt_vals[!is_self_loop],
				wt_vals[!is_self_loop],
				wt_vals[is_self_loop]
			),
			stringsAsFactors = FALSE
		)
		names(result) <- c(actor1, actor2, weight)
	} else {
		# longitudinal
		tm_vals <- agg_result$.time
		result <- data.frame(
			actor1 = c(
				a1_vals[!is_self_loop],
				a2_vals[!is_self_loop],
				a1_vals[is_self_loop]
			),
			actor2 = c(
				a2_vals[!is_self_loop],
				a1_vals[!is_self_loop],
				a2_vals[is_self_loop]
			),
			time = c(
				tm_vals[!is_self_loop],
				tm_vals[!is_self_loop],
				tm_vals[is_self_loop]
			),
			weight = c(
				wt_vals[!is_self_loop],
				wt_vals[!is_self_loop],
				wt_vals[is_self_loop]
			),
			stringsAsFactors = FALSE
		)
		names(result) <- c(actor1, actor2, time, weight)
	}

	return(result)
}
