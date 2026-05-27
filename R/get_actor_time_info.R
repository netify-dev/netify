#' Extract actor time range information
#'
#' `get_actor_time_info` returns a per-actor data.frame of entry and exit times.
#' It dispatches on the first argument:
#'
#' \itemize{
#'   \item If `x` is a **netify object**, it returns the stored `actor_pds`
#'     attribute directly (one row per actor with `min_time` / `max_time`).
#'     This is the open-cohort roster the netlet was built with — and the
#'     roster every per-period statistic (density, degree, homophily) is
#'     computed against.
#'   \item If `x` is a **data.frame** of dyadic observations, it computes the
#'     entry / exit times from the data. Entry is defined as the first time
#'     period in which an actor appears in any interaction (as either sender
#'     or receiver), and exit as the last time period. Use this form to
#'     prepare the `actor_pds` argument to `netify()`.
#' }
#'
#' @param x A netify object, or a data.frame of dyadic observations.
#' @param actor1 Character string specifying the column name for the first
#'   actor in each dyad (data.frame method only).
#' @param actor2 Character string specifying the column name for the second
#'   actor in each dyad (data.frame method only).
#' @param time Character string specifying the column name for time periods
#'   (data.frame method only).
#' @param ... Unused; reserved for future methods.
#'
#' @return A data.frame with three columns:
#'   \itemize{
#'     \item \strong{actor}: Character vector of unique actor identifiers.
#'     \item \strong{min_time}: Earliest time period the actor is in the
#'       network (entry point).
#'     \item \strong{max_time}: Latest time period the actor is in the
#'       network (exit point).
#'   }
#'
#'   For the netify method, this is a verbatim copy of `attr(x, "actor_pds")`.
#'   For the data.frame method, actors are ordered as they appear in the
#'   aggregation, not alphabetically or by time.
#'
#' @details
#' \strong{Use cases:}
#'
#' \itemize{
#'   \item On a **dyad data.frame**: build the `actor_pds` argument to
#'     `netify(..., actor_time_uniform = FALSE, actor_pds = ...)` for
#'     open-cohort panels (panel surveys with attrition, contact-tracing
#'     chains, organizational membership over time, etc.).
#'   \item On a **netify object**: inspect the entry / exit roster the netlet
#'     is currently using — useful when debugging density denominators,
#'     writing custom exporters, or verifying that an open-cohort netlet
#'     has the actor windows you expect.
#' }
#'
#' \strong{Assumptions (data.frame method):}
#' \itemize{
#'   \item An actor is considered "present" in any time period where they
#'     appear in the data, regardless of role (sender/receiver).
#'   \item Missing values in time are ignored when calculating min/max.
#'   \item Actors must appear in at least one non-missing time period.
#' }
#'
#' @note
#' The data.frame method assumes that presence in the data indicates network
#' participation. If actors can be temporarily absent from the network while
#' still being considered members, this method will not capture such gaps —
#' supply an explicit `actor_pds` roster to `netify()` instead.
#'
#' @author Shahryar Minhas, Ha Eun Choi
#'
#' @examples
#' # data.frame input: derive the roster
#' df <- data.frame(
#'     i = c("a", "a", "b", "c"),
#'     j = c("b", "c", "c", "a"),
#'     t = c(1, 2, 2, 3)
#' )
#' get_actor_time_info(df, "i", "j", "t")
#'
#' # netify input: read back the stored roster
#' \dontrun{
#' roster <- data.frame(actor = c("a", "b"), min_time = c(1, 1), max_time = c(3, 4))
#' net <- netify(df, actor1 = "i", actor2 = "j", time = "t",
#'               actor_time_uniform = FALSE, actor_pds = roster)
#' get_actor_time_info(net)
#' }
#'
#' @export get_actor_time_info
get_actor_time_info <- function(x, ...) {
	UseMethod("get_actor_time_info")
}

#' @rdname get_actor_time_info
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export
get_actor_time_info.netify <- function(x, ...) {
	pds <- attr(x, "actor_pds")
	if (is.null(pds)) {
		# cross-sectional netlets carry no actor_pds; synthesize a flat one
		# so callers can rely on a stable return shape
		pds <- actor_pds_from_netlet(x)
	}
	pds
}

#' @rdname get_actor_time_info
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export
get_actor_time_info.data.frame <- function(x, actor1, actor2, time, ...) {
	# input validation
	checkmate::assert_string(actor1)
	checkmate::assert_string(actor2)
	checkmate::assert_string(time)

	# check that specified columns exist in the data
	checkmate::assert_choice(actor1, names(x))
	checkmate::assert_choice(actor2, names(x))
	checkmate::assert_choice(time, names(x))

	# convert to dyadic data.frame
	dyad_data <- data.frame(x, stringsAsFactors = FALSE)

	# restructure data into a nodal format so that we can more easily
	# calculate min and max time points from the data
	a1 <- dyad_data[, c(actor1, time)]
	a2 <- dyad_data[, c(actor2, time)]
	names(a2) <- names(a1)
	nodal <- matrix(NA,
		nrow = (nrow(a1) + nrow(a2)), ncol = 2,
		dimnames = list(NULL, c(actor1, time))
	)
	nodal[1:nrow(a1), ] <- as.matrix(a1)
	nodal[(nrow(a1) + 1):nrow(nodal), ] <- as.matrix(a2)
	rm(a1, a2)

	# get time stats by actor
	actor_year <- tapply(
		nodal[, time], nodal[, actor1], function(z) {
			c(num(min(z, na.rm = TRUE)), num(max(z, na.rm = TRUE)))
		}
	)
	rm(nodal)

	#
	actor_info <- do.call("rbind", actor_year)
	actor_info <- data.frame(actor_info, stringsAsFactors = FALSE)
	actor_info$actor <- rownames(actor_info)
	rownames(actor_info) <- NULL
	names(actor_info) <- c("min_time", "max_time", "actor")
	actor_info <- actor_info[, c("actor", "min_time", "max_time")]

	#
	return(actor_info)
}

#' @rdname get_actor_time_info
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export
get_actor_time_info.default <- function(x, actor1, actor2, time, ...) {
	# coerce tibble / data.table / matrix-like inputs and re-dispatch
	if (is.data.frame(x) || inherits(x, c("tbl_df", "tbl", "data.table"))) {
		return(get_actor_time_info.data.frame(
			as.data.frame(x, stringsAsFactors = FALSE),
			actor1 = actor1, actor2 = actor2, time = time
		))
	}
	cli::cli_abort(c(
		"!" = "{.fn get_actor_time_info} has no method for objects of class {.cls {class(x)[1]}}.",
		"i" = "Pass a {.cls netify} object or a {.cls data.frame} of dyadic observations."
	))
}

#' actor_pds_to_frame
#'
#' This function converts the actor_pds attribute of netify
#' objects into a data.frame at the unit of observation level
#' @param netlet_actor_pds actor pds attributes from netify object
#' @return a data.frame object with actor pd info in a actor-time format
#' @author Colin Henry, Shahryar Minhas
#' @keywords internal
#' @noRd

actor_pds_to_frame <- function(netlet_actor_pds, time_labels = NULL) {
	# iterate through actor rows
	frame <- lapply(1:nrow(netlet_actor_pds), function(ii) {
		# get time range
		time_range <- netlet_actor_pds$min_time[ii]:netlet_actor_pds$max_time[ii]

		# create frame for all pds actor existed
		ii_frame <- expand.grid(netlet_actor_pds$actor[ii], time_range)
		ii_frame <- data.frame(ii_frame, stringsAsFactors = FALSE)
		names(ii_frame) <- c("actor", "time")
		ii_frame$actor <- char(ii_frame$actor)
		
		# convert numeric time to labels if provided
		if (!is.null(time_labels)) {
			# map time_range to time_labels (actual values vs indices)
			if (max(time_range) > length(time_labels)) {
				time_label_nums <- as.numeric(time_labels)
				if (!any(is.na(time_label_nums))) {
					ii_frame$time <- as.character(time_range)
				} else {
					ii_frame$time <- as.character(time_labels[match(time_range, sort(unique(c(netlet_actor_pds$min_time, netlet_actor_pds$max_time))))])
				}
			} else {
				ii_frame$time <- as.character(time_labels[ii_frame$time])
			}
		}
		
		return(ii_frame)
	})

	# combine into a single df and return
	frame <- do.call("rbind", frame)
	return(frame)
}

#' Derive actor periods attribute from netlet
#'
#' @param netlet multilayer netlet object
#' @return data.frame formatted in the same way as the
#' actor_pds attribute
#'
#' @author Shahryar Minhas
#'
#' @keywords internal
#' @noRd

actor_pds_from_netlet <- function(netlet) {
	# get type
	netlet_type <- attr(netlet, "netify_type")

	# get measurements
	msrmnts <- netify_measurements(netlet)

	# cross_sec type is just the unique of
	# row and col actors and time cols set to 1
	if (netlet_type == "cross_sec") {
		actors <- unique(c(msrmnts$row_actors, msrmnts$col_actors))
		actor_pds <- data.frame(
			actor = actors, min_time = 1, max_time = 1,
			stringsAsFactors = FALSE
		)
	}

	# array type is the unique of row and col actors
	# expanded over the time range in the data
	if (netlet_type == "longit_array") {
		frame <- expand.grid(
			actor1 = msrmnts$row_actors,
			actor2 = msrmnts$col_actors,
			time = msrmnts$time
		)
		frame$actor1 <- char(frame$actor1)
		frame$actor2 <- char(frame$actor2)
		actor_pds <- get_actor_time_info(frame, "actor1", "actor2", "time")
		actor_pds$min_time <- char(actor_pds$min_time)
		actor_pds$max_time <- char(actor_pds$max_time)
	}

	# list type is the unique of row and col actors
	# expanded over the time range in the data
	if (netlet_type == "longit_list") {
		# iterate through time ranges and get actors
		# by time period
		time_pds <- unique(msrmnts$time)
		frame <- lapply(time_pds, function(tt) {
			# organize frame
			frame <- expand.grid(
				actor1 = msrmnts$row_actors[[tt]],
				actor2 = msrmnts$col_actors[[tt]],
				time = tt
			)
			frame$actor1 <- char(frame$actor1)
			frame$actor2 <- char(frame$actor2)
			frame$time <- char(frame$time)
			return(frame)
		})
		frame <- do.call("rbind", frame)
		actor_pds <- get_actor_time_info(
			frame, "actor1", "actor2", "time"
		)
	}

	#
	return(actor_pds)
}
