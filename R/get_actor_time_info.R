#' Extract actor time range information
#'
#' `get_actor_time_info` returns a per-actor data.frame of entry and exit times.
#' it dispatches on the first argument:
#'
#' \itemize{
#'   \item if `x` is a **netify object**, it returns the stored `actor_pds`
#'     attribute directly (one row per actor with `min_time` / `max_time`).
#'     this is the open-cohort roster the netlet was built with -- and the
#'     roster every per-period statistic (density, degree, homophily) is
#'     computed against.
#'   \item if `x` is a **data.frame** of dyadic observations, it computes the
#'     entry / exit times from the data. entry is defined as the first time
#'     period in which an actor appears in any interaction (as either sender
#'     or receiver), and exit as the last time period. use this form to
#'     prepare the `actor_pds` argument to `netify()`.
#' }
#'
#' @param x a netify object, or a data.frame of dyadic observations.
#' @param actor1 character string specifying the column name for the first
#'   actor in each dyad (data.frame method only).
#' @param actor2 character string specifying the column name for the second
#'   actor in each dyad (data.frame method only).
#' @param time character string specifying the column name for time periods
#'   (data.frame method only).
#' @param ... unused; reserved for future methods.
#'
#' @return a data.frame with three columns:
#'   \itemize{
#'     \item \strong{actor}: character vector of unique actor identifiers.
#'     \item \strong{min_time}: earliest time period the actor is in the
#'       network (entry point).
#'     \item \strong{max_time}: latest time period the actor is in the
#'       network (exit point).
#'   }
#'
#'   for the netify method, this is a verbatim copy of `attr(x, "actor_pds")`.
#'   for the data.frame method, actors are ordered as they appear in the
#'   aggregation, not alphabetically or by time.
#'
#' @details
#' \strong{use cases:}
#'
#' \itemize{
#'   \item on a **dyad data.frame**: build the `actor_pds` argument to
#'     `netify(..., actor_time_uniform = FALSE, actor_pds = ...)` for
#'     open-cohort panels (panel surveys with attrition, contact-tracing
#'     chains, organizational membership over time, etc.).
#'   \item on a **netify object**: inspect the entry / exit roster the netlet
#'     is currently using -- useful when debugging density denominators,
#'     writing custom exporters, or verifying that an open-cohort netlet
#'     has the actor windows you expect.
#' }
#'
#' \strong{assumptions (data.frame method):}
#' \itemize{
#'   \item an actor is considered "present" in any time period where they
#'     appear in the data, regardless of role (sender/receiver).
#'   \item missing values in time are ignored when calculating min/max.
#'   \item actors must appear in at least one non-missing time period.
#' }
#'
#' @note
#' the data.frame method assumes that presence in the data indicates network
#' participation. if actors can be temporarily absent from the network while
#' still being considered members, this method will not capture such gaps --
#' supply an explicit `actor_pds` roster to `netify()` instead.
#'
#' @author shahryar minhas, ha eun choi
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
#' @author cassy dorff, shahryar minhas
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
#' @author cassy dorff, shahryar minhas
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

	time_values <- dyad_data[[time]]
	if (is.factor(time_values)) {
		time_values <- as.character(time_values)
	}

	# restructure data into a nodal format while preserving the time class
	a1 <- data.frame(
		actor = as.character(dyad_data[[actor1]]),
		time = time_values,
		stringsAsFactors = FALSE
	)
	a2 <- data.frame(
		actor = as.character(dyad_data[[actor2]]),
		time = time_values,
		stringsAsFactors = FALSE
	)
	nodal <- rbind(a1, a2)
	nodal <- nodal[!is.na(nodal$time), , drop = FALSE]
	if (nrow(nodal) == 0L) {
		cli::cli_abort("Actors must appear in at least one non-missing time period.")
	}

	actor_order <- unique(nodal$actor)
	actor_time <- split(
		nodal$time,
		factor(nodal$actor, levels = actor_order),
		drop = TRUE
	)
	min_time <- do.call(c, lapply(actor_time, min, na.rm = TRUE))
	max_time <- do.call(c, lapply(actor_time, max, na.rm = TRUE))

	actor_info <- data.frame(
		actor = names(actor_time),
		min_time = min_time,
		max_time = max_time,
		stringsAsFactors = FALSE
	)
	rownames(actor_info) <- NULL

	#
	return(actor_info)
}

#' @rdname get_actor_time_info
#'
#' @author cassy dorff, shahryar minhas
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
#' this function converts the actor_pds attribute of netify
#' objects into a data.frame at the unit of observation level
#' @param netlet_actor_pds actor pds attributes from netify object
#' @return a data.frame object with actor pd info in a actor-time format
#' @author colin henry, shahryar minhas
#' @keywords internal
#' @noRd

actor_pds_to_frame <- function(netlet_actor_pds, time_labels = NULL) {
	if (is.null(netlet_actor_pds) || nrow(netlet_actor_pds) == 0L) {
		return(data.frame(
			actor = character(),
			time = character(),
			stringsAsFactors = FALSE
		))
	}
	if (!is.null(time_labels)) {
		time_labels <- as.character(time_labels)
	}

	# iterate through actor rows
	frame <- lapply(1:nrow(netlet_actor_pds), function(ii) {
		# get time range
		time_range <- actor_time_range(
			netlet_actor_pds$min_time[ii],
			netlet_actor_pds$max_time[ii],
			time_labels
		)

		# create frame for all pds actor existed
		ii_frame <- expand.grid(netlet_actor_pds$actor[ii], time_range)
		ii_frame <- data.frame(ii_frame, stringsAsFactors = FALSE)
		names(ii_frame) <- c("actor", "time")
		ii_frame$actor <- char(ii_frame$actor)
		ii_frame$time <- as.character(ii_frame$time)
		
		return(ii_frame)
	})

	# combine into a single df and return
	frame <- do.call("rbind", frame)
	return(frame)
}

actor_time_range <- function(min_time, max_time, time_labels = NULL) {
	if (!is.null(time_labels)) {
		min_label <- as.character(min_time)
		max_label <- as.character(max_time)
		start <- match(min_label, time_labels)
		end <- match(max_label, time_labels)
		if (is.na(start) || is.na(end)) {
			min_idx <- suppressWarnings(as.integer(min_time))
			max_idx <- suppressWarnings(as.integer(max_time))
			if (!is.na(min_idx) && !is.na(max_idx) &&
				min_idx >= 1L && max_idx <= length(time_labels)) {
				start <- min_idx
				end <- max_idx
			} else {
				cli::cli_abort(c(
					"x" = "Actor period bounds are not in the netlet time labels.",
					"i" = "Check {.arg actor_pds} or rebuild the netlet with matching time labels."
				))
			}
		}
		if (start > end) {
			cli::cli_abort("Actor period minimum must not occur after its maximum.")
		}
		return(time_labels[seq.int(start, end)])
	}

	if (is.numeric(min_time) || is.integer(min_time)) {
		return(seq.int(as.integer(min_time), as.integer(max_time)))
	}
	if (inherits(min_time, "Date")) {
		return(seq(min_time, max_time, by = "day"))
	}
	if (identical(as.character(min_time), as.character(max_time))) {
		return(as.character(min_time))
	}
	cli::cli_abort(c(
		"x" = "Cannot infer intermediate actor periods from non-numeric bounds without time labels.",
		"i" = "Pass {.arg time_labels} or supply one row per observed actor-period."
	))
}

#' derive actor periods attribute from netlet
#'
#' @param netlet multilayer netlet object
#' @return data.frame formatted in the same way as the
#' actor_pds attribute
#'
#' @author shahryar minhas
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
