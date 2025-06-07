#' Convert igraph, network, socio-matrices/arrays, and lists of each of those object to a netify object
#'
#' `prep_for_netify` takes in a network object (e.g., an igraph or network object) 
#' and outputs a netify object. If the input is already a matrix/array/list of matrices,
#' then it simply calls `new_netify`. Otherwise, it calls the appropriate internal 
#' helper (`decompose_igraph` or `decompose_network`) to extract adjacency matrix 
#' and any nodal/dyadic attributes, and then calls `new_netify`.
#' 
#' @aliases convert.to.netify
#' @param net_obj An R object (e.g., \code{igraph}, \code{network}, socio-matrix/array, or a list of these objects) that you want to convert to a netify object.
#' @param weight Optional. Name of the weight attribute in \code{net_obj} to be used 
#' as the main edge weight in the netify object. Default is \code{NULL}. Important to add  
#' for \code{igraph} and \code{network} objects because they do not have a default weight.
#' @param ... Additional arguments passed to the internal processing functions. 
#' @return a netify object
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @keywords netify
#'
#' @export prep_for_netify

prep_for_netify <- function(
    net_obj,
    weight  = NULL, 
    ...
){

    # capture any relevant user specified args
    # if user specified nodal_data or dyad_data,
    # we will use those instead of the ones extracted from net_obj
    other_args <- list(...)
    user_ndata <- other_args$nodal_data
    user_ddata <- other_args$dyad_data

    # placeholders for final adjacency data, plus nodal/dyadic
    adj_data <- NULL
    nodal_data <- NULL
    dyad_data <- NULL
    is_longitudinal <- FALSE
    is_symmetric <- NULL  # Track symmetry for dyad data conversion

    # Single igraph object
    if (inherits(net_obj, "igraph")) {
      processed <- decompose_igraph(net_obj, weight = weight)
      adj_data <- processed$adj_mat
      nodal_data <- processed$ndata
      dyad_data <- processed$ddata
      weight <- processed$weight
      # Get symmetry directly from igraph
      is_symmetric <- !igraph::is_directed(net_obj)
      
    # Single network object
    } else if (inherits(net_obj, "network")) {
      processed <- decompose_network(net_obj, weight = weight)
      adj_data <- processed$adj_mat
      nodal_data <- processed$ndata
      dyad_data <- processed$ddata
      weight <- processed$weight
      # Get symmetry directly from network
      is_symmetric <- !network::is.directed(net_obj)
      
    # Single matrix
    } else if (is.matrix(net_obj)) {
      adj_data <- net_obj
      # For matrix, we'll check symmetry later if needed
      
    # Array (3D for longitudinal)
    } else if (is.array(net_obj)) {
      adj_data <- net_obj
      is_longitudinal <- length(dim(net_obj)) == 3
      
    # List - need to check what type
    } else if (is.list(net_obj) && length(net_obj) > 0) {
      
      # List of igraph objects
      if (all(vapply(net_obj, inherits, logical(1), "igraph"))) {
        processed_list <- lapply(net_obj, decompose_igraph, weight = weight)
        adj_data <- lapply(processed_list, `[[`, "adj_mat")
        
        # For longitudinal, combine nodal data from all time periods
        nodal_data_list <- lapply(seq_along(processed_list), function(i) {
          nd <- processed_list[[i]]$ndata
          if (!is.null(nd)) {
            # Add time column
            nd$time <- names(net_obj)[i] %||% as.character(i)
            # Rename 'name' to 'actor' for consistency if needed
            if ("name" %in% names(nd) && !"actor" %in% names(nd)) {
              names(nd)[names(nd) == "name"] <- "actor"
            }
            # If actor column doesn't exist, create it
            if (!"actor" %in% names(nd)) {
              nd$actor <- rownames(nd) %||% as.character(seq_len(nrow(nd)))
            }
          }
          nd
        })
        
        # Combine all non-null nodal data
        nodal_data_list <- nodal_data_list[!sapply(nodal_data_list, is.null)]
        if (length(nodal_data_list) > 0) {
          nodal_data <- do.call(rbind, nodal_data_list)
          rownames(nodal_data) <- NULL
        } else {
          nodal_data <- NULL
        }
        
        dyad_data <- lapply(processed_list, `[[`, "ddata")
        weight <- processed_list[[1]]$weight
        is_longitudinal <- TRUE
        # Check symmetry from first graph (assuming all have same directedness)
        is_symmetric <- !igraph::is_directed(net_obj[[1]])
        
      # List of network objects
      } else if (all(vapply(net_obj, inherits, logical(1), "network"))) {
        processed_list <- lapply(net_obj, decompose_network, weight = weight)
        adj_data <- lapply(processed_list, `[[`, "adj_mat")
        
        # For longitudinal, combine nodal data from all time periods
        nodal_data_list <- lapply(seq_along(processed_list), function(i) {
          nd <- processed_list[[i]]$ndata
          if (!is.null(nd)) {
            # Add time column
            nd$time <- names(net_obj)[i] %||% as.character(i)
            # If actor column doesn't exist, create it
            if (!"actor" %in% names(nd)) {
              nd$actor <- rownames(nd) %||% as.character(seq_len(nrow(nd)))
            }
          }
          nd
        })
        
        # Combine all non-null nodal data
        nodal_data_list <- nodal_data_list[!sapply(nodal_data_list, is.null)]
        if (length(nodal_data_list) > 0) {
          nodal_data <- do.call(rbind, nodal_data_list)
          rownames(nodal_data) <- NULL
        } else {
          nodal_data <- NULL
        }
        
        dyad_data <- lapply(processed_list, `[[`, "ddata")
        weight <- processed_list[[1]]$weight
        is_longitudinal <- TRUE
        # Check symmetry from first network
        is_symmetric <- !network::is.directed(net_obj[[1]])
        
      # List of matrices
      } else if (all(vapply(net_obj, is.matrix, logical(1)))) {
        adj_data <- net_obj
        is_longitudinal <- TRUE
        
      # Unsupported list type
      } else {
        cli::cli_alert_danger(
          "Error: List must contain all matrices, all igraph objects, or all network objects.")
        stop()
      }
      
    # Unsupported type
    } else {
      cli::cli_alert_danger(
        "Error: Unsupported class for `net_obj`. Supported classes are: matrix, array, igraph, network, or a list of these objects.")
      stop()
    }

    # Convert dyad_data to the new format if it exists
    if (!is.null(dyad_data)) {
      # For matrix/array cases where symmetry wasn't determined, check now
      if (is.null(is_symmetric)) {
        first_mat <- if (is.list(adj_data)) adj_data[[1]] else adj_data
        is_symmetric <- isSymmetric(first_mat)
      }
      
      dyad_data <- convert_dyad_data_to_new_format(
        dyad_data, adj_data, is_longitudinal, is_symmetric)
    }

    # Validate user-provided nodal_data if given
    if (!is.null(user_ndata)) {
      validate_nodal_data(user_ndata, adj_data, is_longitudinal)
      nodal_data <- user_ndata
    }

    # Validate user-provided dyad_data if given
    if (!is.null(user_ddata)) {
      validate_dyad_data(user_ddata, adj_data, is_longitudinal)
      dyad_data <- user_ddata
    }

    # Ensure consistent actor ordering across all data components
    if (!is.null(nodal_data)) {
      nodal_data <- align_nodal_data_order(nodal_data, adj_data, is_longitudinal)
    }
    
    if (!is.null(dyad_data)) {
      dyad_data <- align_dyad_data_order(dyad_data, adj_data, is_longitudinal)
    }

    # build final netify object
    out <- new_netify(
      data = adj_data,
      weight = weight,
      nodal_data = nodal_data,
      dyad_data = dyad_data,
      ...
    )

    #
    return(out)
}

#' Align nodal data actor order to match adjacency matrix order
#' 
#' This internal function ensures that actors in nodal_data appear in the same 
#' order as they appear in the adjacency matrix row/column names.
#'
#' @param ndata A data frame containing nodal attributes with 'actor' column
#' @param adj_data An adjacency matrix or list of adjacency matrices
#' @param is_longitudinal Logical indicating whether the network is longitudinal
#'
#' @return A reordered data frame with actors matching adjacency matrix order
#'
#' @author Shahryar Minhas
#' 
#' @keywords internal
#' @noRd
align_nodal_data_order <- function(ndata, adj_data, is_longitudinal) {
  
  # Get reference actor order from adjacency matrix
  first_mat <- if (is.list(adj_data)) adj_data[[1]] else adj_data
  ref_actors <- rownames(first_mat)
  
  # Should always have names now, but keep fallback
  if (is.null(ref_actors)) {
    ref_actors <- paste0("a", seq_len(nrow(first_mat)))
  }
  
  if (is_longitudinal) {
    # Use split for better performance than repeated subsetting
    ndata_split <- split(ndata, ndata$time)
    
    # Reorder within each time period
    ndata_ordered <- do.call(rbind, lapply(ndata_split, function(time_data) {
      time_data[match(ref_actors, time_data$actor), ]
    }))
    
    # Reset row names
    rownames(ndata_ordered) <- NULL
    
  } else {
    # Cross-sectional: simple reordering
    ndata_ordered <- ndata[match(ref_actors, ndata$actor), ]
    rownames(ndata_ordered) <- NULL
  }
  
  return(ndata_ordered)
}

#' Align dyad data matrix order to match adjacency matrix order
#' 
#' This internal function ensures that row and column orders in dyad_data matrices
#' match the order of actors in the adjacency matrix.
#'
#' @param ddata A nested list structure containing dyadic attribute matrices
#' @param adj_data An adjacency matrix or list of adjacency matrices
#' @param is_longitudinal Logical indicating whether the network is longitudinal
#'
#' @return A reordered nested list with matrix dimensions matching adjacency matrix order
#'
#' @author Shahryar Minhas
#' 
#' @keywords internal
#' @noRd
align_dyad_data_order <- function(ddata, adj_data, is_longitudinal) {
  
  # For longitudinal data, we need to align each time period separately
  if (is_longitudinal) {
    ddata_ordered <- vector("list", length(ddata))
    names(ddata_ordered) <- names(ddata)
    
    for (t_idx in seq_along(ddata)) {
      t <- names(ddata)[t_idx]
      
      # Get the correct adjacency matrix for this time period
      current_mat <- adj_data[[t_idx]]
      ref_actors <- rownames(current_mat)
      n_actors <- nrow(current_mat)
      
      if (is.null(ref_actors)) {
        ref_actors <- paste0("a", seq_len(n_actors))
      }
      
      time_data <- ddata[[t]]
      ddata_ordered[[t]] <- vector("list", length(time_data))
      names(ddata_ordered[[t]]) <- names(time_data)
      
      # Process each variable
      for (var in names(time_data)) {
        mat <- time_data[[var]]
        
        # Get current actor names
        curr_rows <- rownames(mat)
        curr_cols <- colnames(mat)
        
        # If no names, assume they're in order 1:n
        if (is.null(curr_rows)) curr_rows <- as.character(seq_len(nrow(mat)))
        if (is.null(curr_cols)) curr_cols <- as.character(seq_len(ncol(mat)))
        
        # Reorder to match reference actors
        row_idx <- match(ref_actors, curr_rows)
        col_idx <- match(ref_actors, curr_cols)
        
        # Create reordered matrix
        mat_ordered <- mat[row_idx, col_idx, drop = FALSE]
        dimnames(mat_ordered) <- list(ref_actors, ref_actors)
        
        ddata_ordered[[t]][[var]] <- mat_ordered
      }
    }
  } else {
    # Cross-sectional case
    first_mat <- adj_data
    ref_actors <- rownames(first_mat)
    n_actors <- nrow(first_mat)
    
    if (is.null(ref_actors)) {
      ref_actors <- paste0("a", seq_len(n_actors))
    }
    
    ddata_ordered <- vector("list", length(ddata))
    names(ddata_ordered) <- names(ddata)
    
    # Process each time period (should just be "1")
    for (t in names(ddata)) {
      time_data <- ddata[[t]]
      ddata_ordered[[t]] <- vector("list", length(time_data))
      names(ddata_ordered[[t]]) <- names(time_data)
      
      # Process each variable
      for (var in names(time_data)) {
        mat <- time_data[[var]]
        
        # Get current actor names
        curr_rows <- rownames(mat)
        curr_cols <- colnames(mat)
        
        # If no names, assume they're in order 1:n
        if (is.null(curr_rows)) curr_rows <- as.character(seq_len(nrow(mat)))
        if (is.null(curr_cols)) curr_cols <- as.character(seq_len(ncol(mat)))
        
        # Reorder to match reference actors
        row_idx <- match(ref_actors, curr_rows)
        col_idx <- match(ref_actors, curr_cols)
        
        # Create reordered matrix
        mat_ordered <- mat[row_idx, col_idx, drop = FALSE]
        dimnames(mat_ordered) <- list(ref_actors, ref_actors)
        
        ddata_ordered[[t]][[var]] <- mat_ordered
      }
    }
  }
  
  return(ddata_ordered)
}

#' Convert edge data from igraph/network to new dyad_data format
#' 
#' This internal function converts edge data extracted from igraph or network objects
#' into the standardized dyad_data format used by netify. The output format is a 
#' nested list structure: time periods -> variables -> matrices.
#'
#' @param edge_data A data frame (cross-sectional) or list of data frames (longitudinal) 
#' containing edge attributes with 'from' and 'to' columns
#' @param adj_data An adjacency matrix (cross-sectional) or list of adjacency matrices 
#' (longitudinal) used to determine dimensions and actor names
#' @param is_longitudinal Logical indicating whether the network is longitudinal
#' @param is_symmetric Logical indicating whether the network is symmetric/undirected
#'
#' @return A list with the following structure:
#' \itemize{
#'   \item For cross-sectional: list("1" = list(var1 = matrix, var2 = matrix, ...))
#'   \item For longitudinal: list(time1 = list(var1 = matrix, ...), time2 = list(...), ...)
#' }
#' Returns NULL if edge_data is NULL.
#'
#' @author Shahryar Minhas
#' 
#' @keywords internal
#' @noRd
convert_dyad_data_to_new_format <- function(
    edge_data, adj_data, is_longitudinal, is_symmetric = FALSE) {
  
  # Handle NULL case
  if (is.null(edge_data)) return(NULL)
  
  # Get time periods
  if (is_longitudinal) {
    time_periods <- names(adj_data) %||% as.character(seq_along(adj_data))
  } else {
    time_periods <- "1"
  }
  
  # Pre-allocate the result structure
  dyad_data_new <- vector("list", length(time_periods))
  names(dyad_data_new) <- time_periods
  
  # Process each time period
  for (t_idx in seq_along(time_periods)) {
    t <- time_periods[t_idx]
    
    # Get the appropriate adjacency matrix for this time period
    if (is_longitudinal) {
      current_mat <- adj_data[[t_idx]]
    } else {
      current_mat <- adj_data
    }
    
    # Get dimensions and actor names for THIS time period
    n_actors <- nrow(current_mat)
    actor_names <- rownames(current_mat)
    
    # If no actor names, create default ones
    if (is.null(actor_names)) {
      actor_names <- paste0("a", seq_len(n_actors))
    }
    
    # Create actor lookup for this time period (O(1) lookup vs O(n) with match)
    actor_lookup <- seq_len(n_actors)
    names(actor_lookup) <- actor_names
    
    # Get edge data for this time period
    if (is_longitudinal && is.list(edge_data)) {
      edge_df <- edge_data[[t_idx]]
    } else {
      edge_df <- edge_data
    }
    
    # Skip if no edge data
    if (is.null(edge_df) || nrow(edge_df) == 0) {
      dyad_data_new[[t]] <- list()
      next
    }
    
    # Get variable names (excluding from/to columns)
    var_names <- setdiff(names(edge_df), c("from", "to"))
    
    # If no variables, skip
    if (length(var_names) == 0) {
      dyad_data_new[[t]] <- list()
      next
    }
    
    # Pre-allocate variable list
    dyad_data_new[[t]] <- vector("list", length(var_names))
    names(dyad_data_new[[t]]) <- var_names
    
    # Use fast lookup instead of match
    from_indices <- actor_lookup[as.character(edge_df$from)]
    to_indices <- actor_lookup[as.character(edge_df$to)]
    
    # Find valid edges once
    valid_edges <- which(!is.na(from_indices) & !is.na(to_indices))
    n_valid <- length(valid_edges)
    
    if (n_valid > 0) {
      # Extract valid indices once
      valid_from <- from_indices[valid_edges]
      valid_to <- to_indices[valid_edges]
      
      # If symmetric, pre-compute all indices at once
      if (is_symmetric) {
        all_row_idx <- c(valid_from, valid_to)
        all_col_idx <- c(valid_to, valid_from)
        edge_matrix_idx <- cbind(all_row_idx, all_col_idx)
      } else {
        edge_matrix_idx <- cbind(valid_from, valid_to)
      }
      
      # Create matrices for each variable
      for (var in var_names) {
        # Initialize with zeros (faster than matrix() and matches igraph convention)
        mat <- array(0, dim = c(n_actors, n_actors))
        
        # Get values
        valid_values <- edge_df[[var]][valid_edges]
        
        # Set all values at once using vectorized indexing
        if (is_symmetric) {
          mat[edge_matrix_idx] <- rep(valid_values, 2)
        } else {
          mat[edge_matrix_idx] <- valid_values
        }
        
        # Add dimnames
        dimnames(mat) <- list(actor_names, actor_names)
        
        dyad_data_new[[t]][[var]] <- mat
      }
    } else {
      # No valid edges - create zero matrices
      for (var in var_names) {
        mat <- array(0, dim = c(n_actors, n_actors))
        dimnames(mat) <- list(actor_names, actor_names)
        dyad_data_new[[t]][[var]] <- mat
      }
    }
  }
  
  return(dyad_data_new)
}

#' Validate nodal data structure for netify objects
#' 
#' This internal function validates that user-provided nodal data meets the 
#' requirements for netify objects. It checks for proper data frame structure,
#' required columns, and consistency with the network's actors.
#'
#' @param ndata A data frame containing nodal attributes
#' @param adj_data An adjacency matrix (cross-sectional) or list of adjacency 
#' matrices (longitudinal) to check actor consistency
#' @param is_longitudinal Logical indicating whether the network is longitudinal
#'
#' @return NULL (invisibly). Function stops with error if validation fails.
#'
#' @details 
#' The function performs the following checks:
#' \itemize{
#'   \item ndata must be a data.frame
#'   \item ndata must contain an 'actor' column
#'   \item For longitudinal networks, ndata must contain a 'time' column
#'   \item Warns if actors in the network are missing from nodal_data
#' }
#'
#' @author Shahryar Minhas
#' 
#' @keywords internal
#' @noRd
validate_nodal_data <- function(ndata, adj_data, is_longitudinal) {
  
  if (!is.data.frame(ndata)) {
    cli::cli_alert_danger("nodal_data must be a data.frame")
    stop()
  }
  
  # Check for required columns
  if (!("actor" %in% names(ndata))) {
    cli::cli_alert_danger("nodal_data must contain an 'actor' column")
    stop()
  }
  
  if (is_longitudinal && !("time" %in% names(ndata))) {
    cli::cli_alert_danger(
      "For longitudinal networks, nodal_data must contain a 'time' column")
    stop()
  }
  
  # Check actor names match
  first_mat <- if (is.list(adj_data)) adj_data[[1]] else adj_data
  actor_names <- rownames(first_mat)
  
  if (!is.null(actor_names)) {
    missing_actors <- setdiff(actor_names, ndata$actor)
    if (length(missing_actors) > 0) {
      cli::cli_alert_warning(
        "Some actors in the network are missing from nodal_data: {missing_actors}")
    }
  }
}

#' Validate dyad data structure for netify objects
#' 
#' This internal function validates that user-provided dyad data meets the 
#' requirements for netify objects. It checks for proper nested list structure,
#' matrix dimensions, and consistency with the network's time periods.
#'
#' @param ddata A nested list structure containing dyadic attributes
#' @param adj_data An adjacency matrix (cross-sectional) or list of adjacency 
#' matrices (longitudinal) to check dimensions and time consistency
#' @param is_longitudinal Logical indicating whether the network is longitudinal
#'
#' @return NULL (invisibly). Function stops with error if validation fails.
#'
#' @details 
#' The function validates the following structure:
#' \itemize{
#'   \item ddata must be a list
#'   \item Each time period must be a list of matrices
#'   \item Each matrix must have dimensions matching the number of actors
#'   \item Warns if expected time periods are missing from dyad_data
#' }
#'
#' Expected structure:
#' \itemize{
#'   \item Cross-sectional: list("1" = list(var1 = matrix, var2 = matrix, ...))
#'   \item Longitudinal: list(time1 = list(var1 = matrix, ...), time2 = list(...), ...)
#' }
#'
#' @author Shahryar Minhas
#' 
#' @keywords internal
#' @noRd
validate_dyad_data <- function(ddata, adj_data, is_longitudinal) {
  
  if (!is.list(ddata)) {
    cli::cli_alert_danger("dyad_data must be a list")
    stop()
  }
  
  # For longitudinal, need to check each time period separately
  if (is_longitudinal) {
    expected_times <- names(adj_data) %||% as.character(seq_along(adj_data))
    
    # Check time periods
    ddata_times <- names(ddata)
    missing_times <- setdiff(expected_times, ddata_times)
    if (length(missing_times) > 0) {
      cli::cli_alert_warning(
        "Some time periods are missing from dyad_data: {missing_times}")
    }
    
    # Check structure for each time period
    for (t_idx in seq_along(ddata_times)) {
      t <- ddata_times[t_idx]
      time_data <- ddata[[t]]
      
      # Get the correct number of actors for this time period
      if (t %in% expected_times) {
        t_idx_adj <- which(expected_times == t)
        n_actors <- nrow(adj_data[[t_idx_adj]])
      } else {
        next  # Skip validation for unexpected time periods
      }
      
      if (!is.list(time_data)) {
        cli::cli_alert_danger(
          "Each time period in dyad_data must be a list of matrices")
        stop()
      }
      
      # Check each variable
      for (var in names(time_data)) {
        mat <- time_data[[var]]
        
        if (!is.matrix(mat)) {
          cli::cli_alert_danger(
            "dyad_data[['{t}']][['{var}']] must be a matrix")
          stop()
        }
        
        mat_dims <- dim(mat)
        if (mat_dims[1] != n_actors || mat_dims[2] != n_actors) {
          cli::cli_alert_danger(
            "dyad_data[['{t}']][['{var}']] has incorrect dimensions. Expected {n_actors}x{n_actors}, got {mat_dims[1]}x{mat_dims[2]}")
          stop()
        }
      }
    }
  } else {
    # Cross-sectional case
    n_actors <- nrow(adj_data)
    expected_times <- "1"
    
    # Check time periods
    ddata_times <- names(ddata)
    missing_times <- setdiff(expected_times, ddata_times)
    if (length(missing_times) > 0) {
      cli::cli_alert_warning(
        "Some time periods are missing from dyad_data: {missing_times}")
    }
    
    # Check structure for each time period
    for (t in ddata_times) {
      time_data <- ddata[[t]]
      
      if (!is.list(time_data)) {
        cli::cli_alert_danger(
          "Each time period in dyad_data must be a list of matrices")
        stop()
      }
      
      # Check each variable
      for (var in names(time_data)) {
        mat <- time_data[[var]]
        
        if (!is.matrix(mat)) {
          cli::cli_alert_danger(
            "dyad_data[['{t}']][['{var}']] must be a matrix")
          stop()
        }
        
        mat_dims <- dim(mat)
        if (mat_dims[1] != n_actors || mat_dims[2] != n_actors) {
          cli::cli_alert_danger(
            "dyad_data[['{t}']][['{var}']] has incorrect dimensions. Expected {n_actors}x{n_actors}, got {mat_dims[1]}x{mat_dims[2]}")
          stop()
        }
      }
    }
  }
}

#' NULL-coalescing operator
#' 
#' This internal operator returns the right-hand side value if the left-hand 
#' side is NULL, otherwise returns the left-hand side value. Similar to the 
#' ?? operator in other languages.
#'
#' @param x The primary value to check
#' @param y The fallback value to use if x is NULL
#'
#' @return Returns x if x is not NULL, otherwise returns y
#'
#' @examples
#' # Not run (internal function):
#' # x %||% y  # returns x if x is not NULL, otherwise y
#' # NULL %||% "default"  # returns "default"
#' # "value" %||% "default"  # returns "value"
#'
#' @author Shahryar Minhas
#' 
#' @keywords internal
#' @noRd
`%||%` <- function(x, y) { 
  if (is.null(x)) y else x 
}

#' Break down an `igraph` object into base R components
#'
#' This function processes an igraph object to extract its adjacency matrix along with any available vertex and edge attributes, returning them in a standardized list format.
#'
#' @param grph An igraph object.
#' @param weight An optional character string specifying the edge attribute to use as weights. Defaults to NULL.
#'
#' @return A list containing:
#' \describe{
#'   \item{adj_mat}{The adjacency matrix extracted from the igraph object.}
#'   \item{ndata}{A data frame of vertex attributes, or NULL if not available.}
#'   \item{ddata}{A data frame of edge attributes, or NULL if not available.}
#'   \item{weight}{The edge attribute name used, if provided.}
#' }
#'
#' @details
#' If the graph does not have vertex names, default names are assigned:
#' \itemize{
#'   \item For bipartite graphs: rows are labeled as "r1", "r2", etc., and columns as "c1", "c2", etc.
#'   \item For unipartite graphs: all vertices are labeled as "a1", "a2", etc.
#' }
#' Note: For longitudinal networks with changing actor composition, it is recommended 
#' to explicitly name vertices before conversion to ensure consistent actor identification across time periods.
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export
decompose_igraph <- function(grph, weight = NULL) {
    stopifnot(inherits(grph, "igraph"))

    # Check if bipartite - but also verify the type attribute is actually logical
    is_bipartite <- FALSE
    if (igraph::is_bipartite(grph)) {
        vertex_types <- igraph::V(grph)$type
        # Only treat as bipartite if type attribute exists and is logical
        if (!is.null(vertex_types) && is.logical(vertex_types)) {
            is_bipartite <- TRUE
        } else {
            # Graph thinks it's bipartite but type attribute is invalid
            # Treat as unipartite and warn the user
            cli::cli_warn(c(
                "!" = "Graph has a {.field type} vertex attribute but it's not logical.",
                "i" = "Treating graph as unipartite.",
                ">" = "For bipartite graphs, use logical values: {.code V(g)$type <- c(TRUE, FALSE, ...)}"
            ))
        }
    }
    
    # Get existing vertex names if any
    vertex_names <- igraph::V(grph)$name
    
    if (is_bipartite) {
        # Now we know vertex_types is logical
        n_type1 <- sum(!vertex_types)
        n_type2 <- sum(vertex_types)
        
        # For bipartite graphs, get the bipartite adjacency matrix
        # This returns a matrix of dimension n_type1 x n_type2
        adj_mat <- igraph::as_biadjacency_matrix(grph, attr = weight, sparse = FALSE)
        
        # Create vertex names if they don't exist
        if (is.null(vertex_names)) {
            vertex_names <- character(length(vertex_types))
            vertex_names[!vertex_types] <- paste0("r", seq_len(n_type1))
            vertex_names[vertex_types] <- paste0("c", seq_len(n_type2))
            
            # Set row/column names for the bipartite adjacency matrix
            rownames(adj_mat) <- paste0("r", seq_len(n_type1))
            colnames(adj_mat) <- paste0("c", seq_len(n_type2))
        } else {
            # Use existing names
            rownames(adj_mat) <- vertex_names[!vertex_types]
            colnames(adj_mat) <- vertex_names[vertex_types]
        }
    } else {
        # For unipartite graphs, get the regular adjacency matrix
        adj_mat <- igraph::as_adjacency_matrix(grph, attr = weight, sparse = FALSE)
        
        # Create vertex names if they don't exist
        if (is.null(vertex_names)) {
            n_vertices <- igraph::vcount(grph)
            vertex_names <- paste0("a", seq_len(n_vertices))
            rownames(adj_mat) <- vertex_names
            colnames(adj_mat) <- vertex_names
        } else {
            # Use existing names
            rownames(adj_mat) <- vertex_names
            colnames(adj_mat) <- vertex_names
        }
    }

    # pull out vertex attributes as a data.frame, if any
    v_labs <- igraph::vertex_attr_names(grph)
    if (length(v_labs) > 0) {
        ndata <- igraph::as_data_frame(grph, what = "vertices")
        # Always add vertex names as 'actor' column
        ndata$actor <- vertex_names
    } else {
        ndata <- NULL
    }

    # pull out edge attributes as a data.frame, if any
    e_labs <- igraph::edge_attr_names(grph)
    if (length(e_labs) > 0) {
        ddata <- igraph::as_data_frame(grph, what = "edges")
        # Update from/to to use our naming scheme if they're numeric
        if (is.numeric(ddata$from) && is.numeric(ddata$to)) {
            ddata$from <- vertex_names[ddata$from]
            ddata$to <- vertex_names[ddata$to]
        }
    } else {
        ddata <- NULL
    }

    list(
        adj_mat = adj_mat,
        ndata   = ndata,
        ddata   = ddata,
        weight  = weight
    )
}

#' Break down a `network` object into base R components
#'
#' This function processes a network object from the network package to extract its adjacency matrix along with any available vertex and edge attributes, and returns them in a standardized list format.
#'
#' @param ntwk A network object.
#' @param weight An optional character string specifying the edge attribute to use as weights. Defaults to NULL.
#'
#' @return A list containing:
#' \describe{
#'   \item{adj_mat}{The adjacency matrix extracted from the network object.}
#'   \item{ndata}{A data frame of vertex attributes, or NULL if not available.}
#'   \item{ddata}{A data frame of edge attributes combined with vertex names, or NULL if not available.}
#'   \item{weight}{The edge attribute name used, if provided.}
#' }
#'
#' @details
#' If the network does not have vertex names, default names are assigned:
#' \itemize{
#'   \item For bipartite networks: rows are labeled as "r1", "r2", etc., and columns as "c1", "c2", etc.
#'   \item For unipartite networks: all vertices are labeled as "a1", "a2", etc.
#' }
#' Note: For longitudinal networks with changing actor composition, it is recommended 
#' to explicitly name vertices before conversion to ensure consistent actor identification across time periods.
#' 
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export
decompose_network <- function(ntwk, weight = NULL) {
    stopifnot(inherits(ntwk, "network"))

    # Check if bipartite
    is_bipartite <- network::is.bipartite(ntwk)
    
    # Get or create vertex names
    vertex_names <- network::get.vertex.attribute(ntwk, "vertex.names")

    # Check if vertex names are just the default numeric sequence
    # If so, treat as if they don't exist
    if (!is.null(vertex_names) && 
        is.numeric(vertex_names) && 
        identical(vertex_names, seq_len(network::network.size(ntwk)))) {
        vertex_names <- NULL }

    if (is_bipartite) {
        # Get bipartite partition info
        bip_partition <- network::get.network.attribute(ntwk, "bipartite")
        n_type1 <- bip_partition
        n_type2 <- network::network.size(ntwk) - bip_partition
        
        # Create vertex names if they don't exist
        if (is.null(vertex_names)) {
            vertex_names <- c(paste0("r", seq_len(n_type1)), 
                              paste0("c", seq_len(n_type2)))
        }
        
        # Get the full adjacency matrix first
        adj_mat <- network::as.matrix.network.adjacency(ntwk, attrname = weight)
        
        # Set row/column names
        rownames(adj_mat) <- vertex_names[1:n_type1]
        colnames(adj_mat) <- vertex_names[(n_type1 + 1):(n_type1 + n_type2)]
        
    } else {
        # For unipartite networks, get the regular adjacency matrix
        adj_mat <- network::as.matrix.network.adjacency(ntwk, attrname = weight)
        
        # Create vertex names if they don't exist
        if (is.null(vertex_names)) {
            n_vertices <- network::network.size(ntwk)
            vertex_names <- paste0("a", seq_len(n_vertices))
        }
        
        # Set row/column names
        rownames(adj_mat) <- vertex_names
        colnames(adj_mat) <- vertex_names
    }

    # vertex attributes
    v_labs <- network::list.vertex.attributes(ntwk)
    # Don't include system attributes as real vertex attributes
    system_attrs <- c("vertex.names", "na")
    v_labs <- setdiff(v_labs, system_attrs)
    
    if (length(v_labs) > 0) {
        # Use lapply for better performance than sapply
        ndata_list <- lapply(v_labs, function(attr) {
            network::get.vertex.attribute(ntwk, attr)
        })
        names(ndata_list) <- v_labs
        ndata <- as.data.frame(ndata_list, stringsAsFactors = FALSE)
        
        # Always add vertex names as 'actor' column
        ndata$actor <- vertex_names
    } else {
        ndata <- NULL
    }

    # edge attributes
    e_labs <- network::list.edge.attributes(ntwk)
    if (length(e_labs) > 0) {
        edgelist <- network::as.edgelist(ntwk)
        
        # Use lapply for better performance
        attr_list <- lapply(e_labs, function(attr) {
            network::get.edge.attribute(ntwk, attr)
        })
        names(attr_list) <- e_labs
        attr_df <- as.data.frame(attr_list, stringsAsFactors = FALSE)
        
        ddata <- data.frame(
            from = vertex_names[edgelist[, 1]],
            to   = vertex_names[edgelist[, 2]],
            attr_df,
            stringsAsFactors = FALSE
        )
    } else {
        ddata <- NULL
    }

    list(
        adj_mat = adj_mat,
        ndata   = ndata,
        ddata   = ddata,
        weight  = weight
    )
}