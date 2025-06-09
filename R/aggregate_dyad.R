#' Aggregate dyadic event data by actor pairs
#'
#' `aggregate_dyad` is designed for use with 
#' dyadic event datasets such as those from ACLED or ICEWS. These datasets often contain 
#' multiple interactions between the same pair of actors—e.g., protest events, material 
#' cooperation, or verbal conflict—recorded at high frequency. This function aggregates 
#' such repeated observations into a single summary value per dyad, optionally within 
#' specified time periods. It is particularly useful for preparing network inputs by 
#' collapsing daily or monthly event-level data into actor-to-actor matrices.
#'
#' @param dyad_data A data.frame containing dyadic observations. Must include 
#'   columns for two actors and a weight variable. Will be coerced to data.frame 
#'   if a tibble or data.table is provided.
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
#' @return A data.frame with unique actor pairs (and time periods if specified) 
#'   and their aggregated weight values. The output contains columns:
#'   \itemize{
#'     \item \strong{actor1}: First actor in each dyad (using original column name)
#'     \item \strong{actor2}: Second actor in each dyad (using original column name)
#'     \item \strong{time}: Time period if time parameter was specified (using original column name)
#'     \item \strong{weight}: Aggregated (summed) values for each unique dyad (using original column name)
#'   }
#'
#' @details 
#' The function handles both directed and undirected dyadic aggregation:
#' 
#' \strong{For symmetric (undirected) networks:}
#' 
#' The function uses an efficient aggregation method that:
#' \enumerate{
#'   \item Creates symmetric identifiers for each dyad using `gen_symm_id` (where A-B = B-A)
#'   \item Aggregates values by these symmetric identifiers
#'   \item Expands the results back to directed format for consistency with other netify functions
#' }
#' 
#' This ensures that interactions between actors A and B are combined regardless 
#' of direction, useful for undirected relationships like friendships or alliances.
#' 
#' \strong{For asymmetric (directed) networks:}
#' 
#' Standard aggregation is performed treating each directed dyad separately. This 
#' maintains the distinction between A→B and B→A relationships, which is important 
#' for directed interactions like exports/imports or sender/receiver communications.
#' 
#' \strong{Missing value handling:}
#' 
#' The `ignore_missing` parameter controls how NA values are treated:
#' \itemize{
#'   \item When TRUE: Missing values are excluded from the sum (e.g., sum(10, NA, 20) = 30)
#'   \item When FALSE: Any missing value results in NA for that dyad (e.g., sum(10, NA, 20) = NA)
#' }
#'
#' @note 
#' The function preserves the original column names from the input data.frame in 
#' the output, making it easy to chain operations or merge results.
#' 
#' When symmetric = TRUE, the output still maintains a directed format (separate 
#' rows for A-B and B-A) with identical values for both directions. This ensures 
#' compatibility with other netify functions that expect directed dyadic data.
#' 
#'
#' @examples
#' # Load example data
#' data(icews)
#' 
#' # Example 1: Aggregate multiple events between countries
#' # The icews data contains multiple events per country pair
#' icews_2010 <- icews[icews$year == 2010, ]
#' 
#' # Aggregate directed cooperation events
#' agg_coop <- aggregate_dyad(
#'   dyad_data = icews_2010,
#'   actor1 = "i", 
#'   actor2 = "j",
#'   weight = "verbCoop",
#'   symmetric = FALSE
#' )
#' 
#' # Check reduction in observations
#' nrow(icews_2010)  # Original observations
#' nrow(agg_coop)    # Unique directed dyads
#' 
#' # Example 2: Create symmetric trade volumes
#' trade_data <- data.frame(
#'   exporter = c("USA", "USA", "China", "China", "USA", "China"),
#'   importer = c("China", "China", "USA", "USA", "UK", "UK"),
#'   year = c(2020, 2020, 2020, 2021, 2021, 2021),
#'   trade_value = c(100, 50, 75, 80, 120, 90)
#' )
#' 
#' # Aggregate as total trade between countries (undirected)
#' total_trade <- aggregate_dyad(
#'   dyad_data = trade_data,
#'   actor1 = "exporter",
#'   actor2 = "importer", 
#'   time = "year",
#'   weight = "trade_value",
#'   symmetric = TRUE
#' )
#' 
#' # USA-China trade in 2020: 100+50+75 = 225 (appears in both directions)
#' total_trade[total_trade$year == 2020, ]
#' 
#' # Example 3: Aggregate across all time periods
#' all_time_trade <- aggregate_dyad(
#'   dyad_data = trade_data,
#'   actor1 = "exporter",
#'   actor2 = "importer",
#'   time = NULL,  # Aggregate across all years
#'   weight = "trade_value",
#'   symmetric = FALSE
#' )
#' 
#' # USA total exports to China: 100+50 = 150
#' all_time_trade
#' 
#' # Example 4: Handle missing values
#' trade_data_na <- trade_data
#' trade_data_na$trade_value[2] <- NA
#' 
#' # Ignore missing values (default)
#' agg_ignore_na <- aggregate_dyad(
#'   dyad_data = trade_data_na,
#'   actor1 = "exporter",
#'   actor2 = "importer",
#'   time = "year",
#'   weight = "trade_value",
#'   symmetric = FALSE,
#'   ignore_missing = TRUE
#' )
#' 
#' # Include missing values
#' agg_with_na <- aggregate_dyad(
#'   dyad_data = trade_data_na,
#'   actor1 = "exporter",
#'   actor2 = "importer",
#'   time = "year",
#'   weight = "trade_value",
#'   symmetric = FALSE,
#'   ignore_missing = FALSE
#' )
#' 
#' # Compare results for USA->China in 2020
#' agg_ignore_na[agg_ignore_na$exporter == "USA" & 
#'               agg_ignore_na$importer == "China" & 
#'               agg_ignore_na$year == 2020, ]  # 100 (ignored NA)
#' 
#' agg_with_na[agg_with_na$exporter == "USA" & 
#'             agg_with_na$importer == "China" & 
#'             agg_with_na$year == 2020, ]  # NA
#'
#' @author Shahryar Minhas
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
    ignore_missing = TRUE
) {
    
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
        
        # Custom sum function that handles NAs correctly
        sum_func <- if (ignore_missing) {
            function(x) sum(x, na.rm = TRUE)
        } else {
            function(x) {
                if (any(is.na(x))) NA_real_ else sum(x, na.rm = FALSE)
            }
        }
        
        agg_result <- aggregate(
            as.formula(formula_str),
            data = dyad_data,
            FUN = sum_func,
            na.action = na.pass  # Important: pass NAs through to the function
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
        
        # Custom sum function that handles NAs correctly
        sum_func <- if (ignore_missing) {
            function(x) sum(x, na.rm = TRUE)
        } else {
            function(x) {
                if (any(is.na(x))) NA_real_ else sum(x, na.rm = FALSE)
            }
        }
        
        dyad_data <- aggregate(
            formula_agg,
            data = dyad_data,
            FUN = sum_func,
            na.action = na.pass  # Important: pass NAs through to the function
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

#' Generate symmetric identifiers for dyadic data
#' 
#' `gen_symm_id` creates symmetric identifiers for dyadic data, ensuring that 
#' each unique pair of actors receives the same ID regardless of order. This is 
#' particularly useful for undirected network data where the relationship between 
#' actors A and B is identical to the relationship between B and A.
#' 
#' @param dyad_data A data.frame containing dyadic data with at least two columns
#'   representing actors in each dyad. Will be coerced to data.frame if a tibble 
#'   or data.table is provided.
#' @param actor1 Character string specifying the column name for the first actor 
#'   in each dyad.
#' @param actor2 Character string specifying the column name for the second actor 
#'   in each dyad.
#' @param time Character string specifying the column name for time periods. If 
#'   provided, the time value will be appended to the symmetric ID to create 
#'   unique identifiers for each time period. Set to NULL (default) for 
#'   cross-sectional data.
#'   
#' @return A character vector of symmetric identifiers with the same length as 
#'   the number of rows in dyad_data. Each ID is formatted as:
#'   \itemize{
#'     \item \strong{Without time}: "actor1_actor2" (alphabetically sorted)
#'     \item \strong{With time}: "actor1_actor2_time" (actors alphabetically sorted)
#'   }
#'   
#' @details 
#' The function ensures symmetry by alphabetically sorting actor names before 
#' creating the identifier. This guarantees that:
#' \itemize{
#'   \item The dyad "USA-China" receives the same ID as "China-USA"
#'   \item The dyad "Brazil-Argentina" receives the same ID as "Argentina-Brazil"
#'   \item Actor pairs are consistently ordered regardless of input order
#' }
#' 
#' When a time column is specified, it's appended to the symmetric ID to maintain 
#' unique identifiers across different time periods. This allows for proper 
#' aggregation of longitudinal dyadic data while preserving temporal variation.
#' 
#' 
#' @note 
#' All actor values are converted to character strings before creating IDs to 
#' ensure consistent sorting behavior across different data types.
#' 
#' The underscore character ("_") is used as a separator in the IDs. If your 
#' actor names contain underscores, the IDs will still be unique but may be 
#' harder to parse visually.
#' 
#' This function is primarily used internally by `aggregate_dyad` for efficient 
#' symmetric aggregation, but can be used independently for creating symmetric 
#' dyad identifiers.
#' 
#' @examples
#' # Create example dyadic data
#' trade_df <- data.frame(
#'   from = c("USA", "China", "Russia", "USA", "Brazil", "Argentina"),
#'   to = c("China", "USA", "USA", "Russia", "Argentina", "Brazil"),
#'   trade_value = c(100, 100, 50, 75, 30, 25),
#'   year = c(2020, 2020, 2021, 2021, 2021, 2021)
#' )
#' 
#' # Generate symmetric IDs without time
#' trade_df$symm_id <- gen_symm_id(trade_df, "from", "to")
#' print(trade_df[, c("from", "to", "symm_id")])
#' # Note: USA-China and China-USA both get "China_USA"
#' 
#' # Generate symmetric IDs with time
#' trade_df$symm_id_time <- gen_symm_id(trade_df, "from", "to", "year")
#' print(trade_df[, c("from", "to", "year", "symm_id_time")])
#' # Note: USA-China in 2020 gets "China_USA_2020"
#' 
#' # Use for aggregation of undirected relationships
#' trade_df$total_trade <- ave(
#'   trade_df$trade_value, 
#'   trade_df$symm_id_time, 
#'   FUN = sum
#' )
#' print(unique(trade_df[, c("symm_id_time", "total_trade")]))
#' 
#' # Example with longitudinal data
#' library(netify)
#' data(icews)
#' icews_sample <- icews[1:100, ]
#' 
#' # Create symmetric IDs for conflict events
#' icews_sample$symm_dyad <- gen_symm_id(
#'   icews_sample, 
#'   actor1 = "i", 
#'   actor2 = "j", 
#'   time = "year"
#' )
#' 
#' # Check that symmetric pairs get same ID
#' icews_sample[icews_sample$i == "United States" & icews_sample$j == "Israel", "symm_dyad"]
#' icews_sample[icews_sample$i == "Israel" & icews_sample$j == "United States", "symm_dyad"]
#' 
#' @author Shahryar Minhas
#' 
#' @export gen_symm_id

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