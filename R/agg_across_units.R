#' Aggregate dyadic data by actor pairs
#'
#' agg_across_units` (also available as `aggregate_dyad`, `dyad_aggregate`, 
#' `agg_dyad`, and `dyad_agg`) aggregates 
#' dyadic (pairwise) data by summing values across 
#' repeated actor pairs. This function is useful for consolidating multiple 
#' interactions between the same pair of actors into a single summary value,
#' such as combining multiple trade transactions or communication events.
#'
#' @param dyad_data A data frame containing dyadic observations with at least 
#'   columns for two actors and a weight variable.
#' @param actor1 Character string specifying the column name for the first actor 
#'   in each dyad.
#' @param actor2 Character string specifying the column name for the second actor 
#'   in each dyad.
#' @param time Character string specifying the column name for time periods. 
#'   If NULL (default), aggregation is performed across all time periods.
#' @param weight Character string specifying the column name containing values 
#'   to be aggregated (summed) for each unique actor pair.
#' @param symmetric Logical. If TRUE, treats dyads as undirected (i.e., the dyad 
#'   A-B is treated as identical to B-A). If FALSE, treats dyads as directed 
#'   (i.e., A-B is distinct from B-A).
#' @param ignore_missing Logical. If TRUE (default), missing values in the weight 
#'   variable are ignored during aggregation. If FALSE, any dyad containing a 
#'   missing value will result in NA for that aggregated dyad.
#'
#' @return A data frame with unique actor pairs (and time periods if specified) 
#'   and their aggregated weight values. The output will have columns:
#'   \itemize{
#'     \item \code{actor1}: First actor in each dyad
#'     \item \code{actor2}: Second actor in each dyad  
#'     \item \code{time}: Time period (if time parameter was specified)
#'     \item \code{weight}: Aggregated (summed) values for each unique dyad
#'   }
#'
#' @details 
#' For symmetric networks, the function uses an efficient aggregation method that:
#' \enumerate{
#'   \item Creates symmetric identifiers for each dyad (where A-B = B-A)
#'   \item Aggregates values by these identifiers
#'   \item Expands the results back to directed format for consistency
#' }
#' 
#' For asymmetric networks, standard aggregation is performed treating each
#' directed dyad separately.
#'
#' @examples
#' # Create example dyadic trade data
#' trade_data <- data.frame(
#'   exporter = c("USA", "USA", "China", "China", "USA", "China"),
#'   importer = c("China", "China", "USA", "USA", "UK", "UK"),
#'   year = c(2020, 2020, 2020, 2021, 2021, 2021),
#'   trade_value = c(100, 50, 75, 80, 120, 90)
#' )
#' 
#' # Aggregate directed trade flows by year
#' agg_trade <- agg_across_units(
#'   dyad_data = trade_data,
#'   actor1 = "exporter", 
#'   actor2 = "importer",
#'   time = "year",
#'   weight = "trade_value",
#'   symmetric = FALSE
#' )
#' print(agg_trade)
#' 
#' # Aggregate as undirected trade (total trade between countries)
#' total_trade <- agg_across_units(
#'   dyad_data = trade_data,
#'   actor1 = "exporter",
#'   actor2 = "importer", 
#'   time = "year",
#'   weight = "trade_value",
#'   symmetric = TRUE
#' )
#' print(total_trade)
#' 
#' # Aggregate across all years
#' all_years_trade <- agg_across_units(
#'   dyad_data = trade_data,
#'   actor1 = "exporter",
#'   actor2 = "importer",
#'   time = NULL,  # Aggregate across all time periods
#'   weight = "trade_value",
#'   symmetric = FALSE
#' )
#' print(all_years_trade)
#' 
#' @author Shahryar Minhas
#'
#' @importFrom stats na.omit aggregate as.formula
#' 
#' @export agg_across_units
#' @aliases aggregate_dyad dyad_aggregate agg_dyad dyad_agg

agg_across_units <- function(
    dyad_data,
    actor1,
    actor2,
    time = NULL,
    weight,
    symmetric, 
    ignore_missing = TRUE
) {
    
    # add in logicals for how to treat NAs
    if (ignore_missing) {
        remove_nas <- TRUE
        na_action_behav <- NULL
    } else {
        remove_nas <- FALSE
        na_action_behav <- na.omit
    }
    
    # if symmetric, use gen_symm_id for more efficient aggregation
    if (symmetric) {
        # create symmetric ids
        dyad_data$symm_id <- gen_symm_id(
            dyad_data, 
            actor1, 
            actor2, 
            time = time
        )
        
        # aggregate by symmetric id
        formula_str <- paste(weight, "~ symm_id")
        agg_result <- aggregate(
            as.formula(formula_str),
            data = dyad_data,
            FUN = sum,
            na.rm = remove_nas,
            na.action = na_action_behav
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
        # for non-symmetric, use standard aggregation
        if (is.null(time)) {
            formula_agg <- as.formula(paste(weight, "~", actor1, "+", actor2))
        } else {
            formula_agg <- as.formula(paste(weight, "~", actor1, "+", actor2, "+", time))
        }
        
        dyad_data <- aggregate(
            formula_agg,
            data = dyad_data,
            FUN = sum,
            na.rm = remove_nas,
            na.action = na_action_behav
        )
        
        # ensure correct column order
        if (is.null(time)) {
            dyad_data <- dyad_data[, c(actor1, actor2, weight)]
        } else {
            dyad_data <- dyad_data[, c(actor1, actor2, time, weight)]
        }
    }
    
    # reset row names
    rownames(dyad_data) <- NULL
    
    return(dyad_data)
}

#' @rdname agg_across_units
#' @export
aggregate_dyad <- agg_across_units

#' @rdname agg_across_units
#' @export
dyad_aggregate <- agg_across_units

#' @rdname agg_across_units
#' @export
agg_dyad <- agg_across_units

#' @rdname agg_across_units
#' @export
dyad_agg <- agg_across_units


#' Generate Symmetric Identifiers for Dyadic Data
#' 
#' This function creates symmetric identifiers for dyadic data, ensuring that 
#' each unique pair of actors receives the same ID regardless of order. This is 
#' particularly useful for undirected network data where the relationship between 
#' actors A and B is the same as between B and A.
#' 
#' @param dyad_data A data.frame containing dyadic data with at least two columns
#'   representing actors in each dyad.
#' @param actor1 Character string specifying the name of the column containing 
#'   the first actor in each dyad.
#' @param actor2 Character string specifying the name of the column containing 
#'   the second actor in each dyad.
#' @param time Optional character string specifying the name of the column 
#'   containing time information. If provided, the time value will be appended 
#'   to the symmetric ID to create unique identifiers for each time period.
#'   Default is NULL.
#'   
#' @return A character vector of symmetric identifiers with the same length as 
#'   the number of rows in \code{dyad_data}. Each ID is formatted as:
#'   \itemize{
#'     \item Without time: "actor1_actor2" (alphabetically sorted)
#'     \item With time: "actor1_actor2_time" (actors alphabetically sorted)
#'   }
#'   
#' @details 
#' The function ensures symmetry by alphabetically sorting the actor names before 
#' creating the ID. This means that the dyad "USA-China" will receive the same 
#' ID as "China-USA". When a time column is specified, it's appended to maintain 
#' unique identifiers across different time periods.
#' 
#' Performance note: This implementation uses vectorized operations for 
#' significantly better performance on large datasets compared to row-wise operations.
#' 
#' @examples
#' # Create example dyadic data
#' dyad_df <- data.frame(
#'   from = c("USA", "China", "Russia", "USA"),
#'   to = c("China", "USA", "USA", "Russia"),
#'   trade = c(100, 100, 50, 75),
#'   year = c(2020, 2020, 2021, 2021)
#' )
#' 
#' # Generate symmetric IDs without time
#' dyad_df$symm_id <- gen_symm_id(dyad_df, "from", "to")
#' # Results in: "China_USA", "China_USA", "Russia_USA", "Russia_USA"
#' 
#' # Generate symmetric IDs with time
#' dyad_df$symm_id_time <- gen_symm_id(dyad_df, "from", "to", "year")
#' # Results in: "China_USA_2020", "China_USA_2020", "Russia_USA_2021", "Russia_USA_2021"
#' 
#' @author Shahryar Minhas
#' 
#' @export
gen_symm_id <- function(
    dyad_data, 
    actor1, 
    actor2, 
    time = NULL
) {
    
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
    
    # extract actor columns
    a1 <- as.character(dyad_data[[actor1]])
    a2 <- as.character(dyad_data[[actor2]])
    
    # vectorized approach to create sorted actor pairs
    # this is much faster than using apply() row-by-row
    # use pmin/pmax for element-wise min/max comparison
    actor_min <- pmin(a1, a2)
    actor_max <- pmax(a1, a2)
    
    # create base symmetric id
    symm_id <- paste(actor_min, actor_max, sep = "_")
    
    # append time if specified
    if (!is.null(time)) {
        time_vals <- as.character(dyad_data[[time]])
        symm_id <- paste(symm_id, time_vals, sep = "_")
    }
    
    return(symm_id)
}


#' Expand symmetric dyads back to directed format
#' 
#' This internal function takes aggregated symmetric dyads and expands them
#' back to directed format (both A->B and B->A directions).
#'
#' @param agg_result Aggregated data with symm_id column
#' @param actor1 Name for actor1 column
#' @param actor2 Name for actor2 column  
#' @param time Name for time column (NULL if cross-sectional)
#' @param weight Name for weight column
#'
#' @return Data frame with expanded dyads
#'
#' @author Shahryar Minhas
#' 
#' @keywords internal
#' @noRd
expand_symmetric_dyads <- function(agg_result, actor1, actor2, time, weight) {
    
    # split symmetric id back into components
    id_parts <- strsplit(agg_result$symm_id, "_", fixed = TRUE)
    n_dyads <- length(id_parts)
    
    # extract all parts at once for efficiency
    parts_matrix <- matrix(unlist(id_parts), ncol = if(is.null(time)) 2 else 3, byrow = TRUE)
    a1_vals <- parts_matrix[, 1]
    a2_vals <- parts_matrix[, 2]
    wt_vals <- agg_result[[weight]]
    
    # identify self-loops
    is_self_loop <- a1_vals == a2_vals
    n_regular <- sum(!is_self_loop)
    n_self <- sum(is_self_loop)
    
    # calculate total rows needed
    total_rows <- 2 * n_regular + n_self
    
    # build result vectors
    if (is.null(time)) {
        # cross-sectional case
        result <- data.frame(
            actor1 = c(
                a1_vals[!is_self_loop],  # regular dyads, direction 1
                a2_vals[!is_self_loop],  # regular dyads, direction 2
                a1_vals[is_self_loop]    # self-loops
            ),
            actor2 = c(
                a2_vals[!is_self_loop],  # regular dyads, direction 1
                a1_vals[!is_self_loop],  # regular dyads, direction 2
                a2_vals[is_self_loop]    # self-loops
            ),
            weight = c(
                wt_vals[!is_self_loop],  # regular dyads, direction 1
                wt_vals[!is_self_loop],  # regular dyads, direction 2
                wt_vals[is_self_loop]    # self-loops
            ),
            stringsAsFactors = FALSE
        )
        names(result) <- c(actor1, actor2, weight)
    } else {
        # longitudinal case
        tm_vals <- parts_matrix[, 3]
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