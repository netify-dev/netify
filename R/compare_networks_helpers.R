# Helper functions for compare_networks
# This file contains internal functions used by compare_networks()

#' Compare Edge Patterns Between Networks
#'
#' `compare_edges` performs pairwise comparisons of edge patterns between networks using multiple similarity metrics including correlation, Jaccard similarity, Hamming distance, and QAP tests.
#'
#' @param nets_list A list of netify objects to compare.
#' @param method Character string specifying comparison method(s): "correlation", "jaccard", "hamming", "qap", or "all".
#' @param test Logical; whether to perform significance testing using QAP.
#' @param n_permutations Integer; number of permutations for QAP test.
#' @param include_diagonal Logical; whether to include diagonal values in comparison.
#' @param edge_threshold Numeric or function; threshold for determining edge presence in weighted networks.
#' @param return_details Logical; whether to return detailed comparison matrices.
#'
#' @return A list containing comparison results including summary statistics, edge changes, and optionally detailed matrices.
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @keywords internal
#' @noRd

compare_edges <- function(
   nets_list, method, test, n_permutations, 
   include_diagonal, edge_threshold, return_details
   ){
  
   # grab network names if they exist
   net_names <- names(nets_list)
   if (is.null(net_names)) {
       net_names <- paste0("net", seq_along(nets_list))
   }
   
   # set up comparison matrices for all the metrics
   n_nets <- length(nets_list)
   correlation_mat <- matrix(NA, n_nets, n_nets, dimnames = list(net_names, net_names))
   jaccard_mat <- matrix(NA, n_nets, n_nets, dimnames = list(net_names, net_names))
   hamming_mat <- matrix(NA, n_nets, n_nets, dimnames = list(net_names, net_names))
   qap_mat <- matrix(NA, n_nets, n_nets, dimnames = list(net_names, net_names))
   qap_pval_mat <- matrix(NA, n_nets, n_nets, dimnames = list(net_names, net_names))
   spectral_mat <- matrix(NA, n_nets, n_nets, dimnames = list(net_names, net_names))
   
   # track edge changes between networks
   edge_changes <- list()
   
   # pre-compute all actors for alignment efficiency
   all_actors <- sort(unique(unlist(lapply(nets_list, function(net) {
       mat <- extract_matrix(net)
       c(rownames(mat), colnames(mat))
   }))))
   
   # pre-align all matrices if we have many networks
   if (n_nets > 5) {
       aligned_list <- lapply(nets_list, function(net) {
           align_to_actors(net, all_actors, include_diagonal)
       })
   }
   
    # compare all pairs of networks
    for (i in 1:(n_nets-1)) {
        for (j in (i+1):n_nets) {
            # check if networks have different loop settings
            loops1 <- attributes(nets_list[[i]])$loops %||% FALSE
            loops2 <- attributes(nets_list[[j]])$loops %||% FALSE
            
            # if networks have different loop settings, we need to include diagonal
            force_include_diagonal <- include_diagonal || (loops1 != loops2)
            
            # get aligned matrices
            if (n_nets > 5) {
                mat1 <- aligned_list[[i]]
                mat2 <- aligned_list[[j]]
            } else {
                # align the matrices so they have same actors
                mats <- align_matrices(nets_list[[i]], nets_list[[j]], force_include_diagonal)
                mat1 <- mats$mat1
                mat2 <- mats$mat2
            }
           
           # figure out edge thresholds
           if (is.function(edge_threshold)) {
               threshold1 <- edge_threshold(mat1)
               threshold2 <- edge_threshold(mat2)
           } else {
               threshold1 <- threshold2 <- edge_threshold
           }
           
            # calculate similarity metrics based on method
            if (method %in% c("correlation", "all")) {
                # flatten matrices and calc correlation
                vec1 <- as.vector(mat1)
                vec2 <- as.vector(mat2)
                complete <- !is.na(vec1) & !is.na(vec2)
                if (sum(complete) >= 3) {
                    # check for constant vectors (e.g., all zeros or all ones)
                    var1 <- var(vec1[complete])
                    var2 <- var(vec2[complete])
                    
                    if (var1 > 0 && var2 > 0) {
                        correlation_mat[i,j] <- correlation_mat[j,i] <- 
                            correlation_cpp(vec1[complete], vec2[complete])
                    } else if (var1 == 0 && var2 == 0) {
                        # both constant - if same value then perfect correlation, else 0
                        if (all(vec1[complete] == vec1[complete][1]) && 
                            all(vec2[complete] == vec2[complete][1]) &&
                            vec1[complete][1] == vec2[complete][1]) {
                            correlation_mat[i,j] <- correlation_mat[j,i] <- 1
                        } else {
                            correlation_mat[i,j] <- correlation_mat[j,i] <- 0
                        }
                    } else {
                        # one constant, one varying - correlation is 0
                        correlation_mat[i,j] <- correlation_mat[j,i] <- 0
                    }
                } else {
                    # not enough data points - set to 0 for empty networks
                    if (sum(vec1 != 0, na.rm = TRUE) == 0 || sum(vec2 != 0, na.rm = TRUE) == 0) {
                        correlation_mat[i,j] <- correlation_mat[j,i] <- 0
                    }
                }
            }
           
           if (method %in% c("jaccard", "all")) {
               jaccard_mat[i,j] <- jaccard_mat[j,i] <- 
                   calculate_jaccard_fast(mat1, mat2, threshold1, threshold2)
           }
           
           if (method %in% c("hamming", "all")) {
               hamming_mat[i,j] <- hamming_mat[j,i] <- 
                   calculate_hamming_fast(mat1, mat2, threshold1, threshold2)
           }
           
           if (method %in% c("qap", "all") && test) {
               qap_result <- qap_correlation_fast(mat1, mat2, n_permutations)
               qap_mat[i,j] <- qap_mat[j,i] <- qap_result$correlation
               qap_pval_mat[i,j] <- qap_pval_mat[j,i] <- qap_result$p_value
           }
           
           if (method %in% c("spectral", "all")) {
               spectral_mat[i,j] <- spectral_mat[j,i] <- 
                   calculate_spectral_distance(mat1, mat2)
           }
           
           # track what edges changed between these two networks
           edge_key <- paste(net_names[i], net_names[j], sep = "_vs_")
           edge_changes[[edge_key]] <- calculate_edge_changes_fast(mat1, mat2, threshold1, threshold2)
       }
   }
   
   # fill in diagonals
   diag(correlation_mat) <- 1
   diag(jaccard_mat) <- 1
   diag(hamming_mat) <- 0
   diag(spectral_mat) <- 0
   if (test) {
       diag(qap_mat) <- 1
       diag(qap_pval_mat) <- 0
   }
   
   # create summary dataframe
   summary_df <- create_edge_summary(correlation_mat, jaccard_mat, hamming_mat, 
                                    qap_mat, qap_pval_mat, spectral_mat, method)
   
    # build results list - make sure n_networks is included!
    results <- list(
        # comparison_type = "edges",
        method = method,
        n_networks = n_nets,  
        summary = summary_df,
        edge_changes = edge_changes
    )
    
    # add significance tests if requested
    if (test) {
        results$significance_tests <- list(
            qap_correlations = qap_mat,
            qap_pvalues = qap_pval_mat
        )
    }
    
    # add detailed matrices if user wants them
    if (return_details) {
        # always return full matrices but ensure they're properly named
        results$details <- list(
            correlation_matrix = correlation_mat,
            jaccard_matrix = jaccard_mat,
            hamming_matrix = hamming_mat,
            spectral_matrix = spectral_mat
        )
    }
    
    return(results)
}

#' Compare Structural Properties Between Networks
#'
#' `compare_structure` calculates and compares network-level structural properties such as density, reciprocity, transitivity, and mean degree across networks.
#'
#' @param nets_list A list of netify objects to compare.
#' @param test Logical; whether to perform significance testing (currently not implemented).
#'
#' @return A list containing structural properties for each network and percent changes if comparing two networks.
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @keywords internal
#' @noRd

compare_structure <- function(nets_list, test){
    
    # get network names
    net_names <- names(nets_list)
    if (is.null(net_names)) {
        net_names <- paste0("net", seq_along(nets_list))
    }
    
    # calc structural properties for each network using summary()
    struct_props <- lapply(seq_along(nets_list), function(i) {
        net <- nets_list[[i]]
        
        # use netify's built-in summary function
        net_summary <- summary(net)
        
        # extract matrix to calculate additional metrics if needed
        mat <- extract_matrix(net)
        n_actors <- nrow(mat)
        n_edges <- net_summary$num_edges %||% sum(mat > 0, na.rm = TRUE)
        
        # calculate mean degree based on network type
        is_directed <- !(attributes(net)$symmetric %||% FALSE)
        if (is_directed) {
            mean_deg <- n_edges / n_actors
        } else {
            # for undirected, edges are counted once but each contributes to 2 degrees
            mean_deg <- (2 * n_edges) / n_actors
        }
        
        # pull out key structural properties
        props <- data.frame(
            network = net_names[i],
            n_nodes = n_actors,
            n_edges = n_edges,
            density = net_summary$density %||% (n_edges / (n_actors * (n_actors - 1))),
            reciprocity = net_summary$reciprocity %||% NA,
            transitivity = net_summary$transitivity %||% NA,
            mean_degree = mean_deg,
            stringsAsFactors = FALSE
        )
        
        # add centralization if available
        if (!is.null(net_summary$competition)) {
            props$centralization <- net_summary$competition
        }
        
        props
    })
    
    # combine into single dataframe
    struct_df <- do.call(rbind, struct_props)
    
    # if only 2 networks, calculate changes between them
    if (length(nets_list) == 2) {
        props1 <- struct_props[[1]]
        props2 <- struct_props[[2]]
        
        # calc changes for all numeric columns
        numeric_cols <- setdiff(names(props1), c("network"))
        changes <- data.frame(
            metric = numeric_cols,
            value_net1 = unlist(props1[numeric_cols]),
            value_net2 = unlist(props2[numeric_cols]),
            stringsAsFactors = FALSE
        )
        changes$absolute_change <- changes$value_net2 - changes$value_net1
        changes$percent_change <- ifelse(changes$value_net1 != 0 & !is.na(changes$value_net1),
                                       100 * changes$absolute_change / changes$value_net1,
                                       NA)
        
        struct_df <- list(
            properties = struct_df,
            changes = changes
        )
    }
    
    # build results 
    results <- list(
        # comparison_type = "structure",
        method = "structural_comparison",
        n_networks = length(nets_list),  
        # if struct_df is a list (with properties and changes), use properties
        summary = if (is.list(struct_df) && "properties" %in% names(struct_df)) {
            struct_df$properties
        } else {
            struct_df
        }
    )
    
    # if we have changes data (2 networks), add it separately
    if (is.list(struct_df) && "changes" %in% names(struct_df)) {
        results$changes <- struct_df$changes
    }
    
    return(results)
}

#' Compare Node Composition Between Networks
#'
#' `compare_nodes` tracks which actors are present in each network and calculates node overlap statistics including Jaccard similarity of node sets.
#'
#' @param nets_list A list of netify objects to compare.
#' @param return_details Logical; whether to return detailed node sets and overlap matrices.
#'
#' @return A list containing node composition summary and changes between networks.
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @keywords internal
#' @noRd

compare_nodes <- function(nets_list, return_details = FALSE){
  
   # get network names
   net_names <- names(nets_list)
   if (is.null(net_names)) {
       net_names <- paste0("net", seq_along(nets_list))
   }
   
   # extract node sets from each network
   node_sets <- lapply(nets_list, function(net) {
       mat <- extract_matrix(net)
       unique(c(rownames(mat), colnames(mat)))
   })
   
   # set up matrices to track node overlap
   n_nets <- length(nets_list)
   overlap_mat <- matrix(NA, n_nets, n_nets, dimnames = list(net_names, net_names))
   jaccard_node_mat <- matrix(NA, n_nets, n_nets, dimnames = list(net_names, net_names))
   
   # track node changes
   node_changes <- list()
   
   # compare all pairs
   for (i in 1:(n_nets-1)) {
       for (j in (i+1):n_nets) {
           nodes1 <- node_sets[[i]]
           nodes2 <- node_sets[[j]]
           
           # calc overlap
           common <- length(intersect(nodes1, nodes2))
           total <- length(union(nodes1, nodes2))
           
           overlap_mat[i,j] <- overlap_mat[j,i] <- common
           jaccard_node_mat[i,j] <- jaccard_node_mat[j,i] <- 
               if(total > 0) common / total else 0
           
           # track what changed
           node_key <- paste(net_names[i], net_names[j], sep = "_vs_")
           node_changes[[node_key]] <- list(
               added = setdiff(nodes2, nodes1),
               removed = setdiff(nodes1, nodes2),
               maintained = intersect(nodes1, nodes2),
               n_added = length(setdiff(nodes2, nodes1)),
               n_removed = length(setdiff(nodes1, nodes2)),
               n_maintained = common
           )
       }
   }
   
   # fill diagonals
   diag(overlap_mat) <- sapply(node_sets, length)
   diag(jaccard_node_mat) <- 1
   
   # create summary
   if (n_nets == 2) {
       summary_df <- data.frame(
           comparison = paste(net_names[1], "vs", net_names[2]),
           nodes_net1 = length(node_sets[[1]]),
           nodes_net2 = length(node_sets[[2]]),
           common_nodes = overlap_mat[1,2],
           jaccard_similarity = jaccard_node_mat[1,2],
           nodes_added = node_changes[[1]]$n_added,
           nodes_removed = node_changes[[1]]$n_removed,
           stringsAsFactors = FALSE
       )
   } else {
       # summary for multiple networks
       summary_df <- data.frame(
           network = net_names,
           n_nodes = sapply(node_sets, length),
           mean_overlap = rowMeans(overlap_mat, na.rm = TRUE),
           mean_jaccard = rowMeans(jaccard_node_mat, na.rm = TRUE) - 1/(n_nets),
           stringsAsFactors = FALSE
       )
   }
   
   results <- list(
       # comparison_type = "nodes",
       method = "node_composition",
       n_networks = n_nets,  
       summary = summary_df,
       node_changes = node_changes
   )
   
   if (return_details) {
       results$details <- list(
           overlap_matrix = overlap_mat,
           jaccard_matrix = jaccard_node_mat,
           node_sets = node_sets
       )
   }
   
   return(results)
}

#' Compare Nodal Attribute Distributions Between Networks
#'
#' `compare_attributes` compares the distributions of nodal attributes across networks using correlation for continuous attributes and total variation distance for categorical attributes.
#'
#' @param nets_list A list of netify objects to compare.
#' @param test Logical; whether to perform KS tests for continuous attributes.
#' @param n_permutations Integer; number of permutations for significance testing (currently unused).
#' @param return_details Logical; whether to return detailed attribute values and test matrices.
#'
#' @return A list containing attribute similarity comparisons across networks.
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @keywords internal
#' @noRd

compare_attributes <- function(
   nets_list, test = TRUE, n_permutations = 1000, 
   return_details = FALSE
   ){
  
   # get network names
   net_names <- names(nets_list)
   if (is.null(net_names)) {
       net_names <- paste0("net", seq_along(nets_list))
   }
   
   # extract nodal attributes from each network
   attrs_list <- lapply(nets_list, function(net) {
       attrs <- attributes(net)
       attrs$nodal_data
   })
   
   # find common attributes across networks
   all_attr_names <- unique(unlist(lapply(attrs_list, names)))
   common_attrs <- setdiff(all_attr_names, c("actor", "time", "layer"))
   
   if (length(common_attrs) == 0) {
       cli::cli_alert_warning("No common nodal attributes found across networks")
       return(list(
           # comparison_type = "attributes",
           method = "attribute_comparison",
           n_networks = length(nets_list),  
           summary = data.frame(message = "No common attributes to compare")
       ))
   }
   
   # compare networks based on each attribute
   attr_comparisons <- list()
   
   for (attr in common_attrs) {
       attr_result <- compare_single_attribute(nets_list, attrs_list, attr, 
                                             test, n_permutations)
       attr_comparisons[[attr]] <- attr_result
   }
   
   # compile summary
   summary_list <- lapply(names(attr_comparisons), function(attr) {
       comp <- attr_comparisons[[attr]]
       if (!is.null(comp$summary)) {
           cbind(attribute = attr, comp$summary)
       } else {
           NULL
       }
   })
   
   summary_df <- do.call(rbind, summary_list[!sapply(summary_list, is.null)])
   
    results <- list(
        # comparison_type = "attributes", 
        method = "attribute_comparison",
        n_networks = length(nets_list),  
        summary = summary_df,
        by_attribute = attr_comparisons
    )
   
   if (return_details) {
       results$details <- lapply(attr_comparisons, function(x) x$details)
   }
   
   return(results)
}

# helper functions that were missing or need to be kept:

#' Extract Network List from Netify Object
#'
#' `extract_network_list` converts different netify object types (cross-sectional, longitudinal array, or longitudinal list) into a standardized list format for comparison.
#'
#' @param net A netify object of any type.
#'
#' @return A list of netify objects, one for each time period or layer.
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @keywords internal
#' @noRd

extract_network_list <- function(net){

   #
   attrs <- attributes(net)
   
   # check if this is a multilayer network
   has_layers <- !is.null(attrs$layers)
   
   if (attrs$netify_type == "longit_list") {
       # for longitudinal list, check if it's multilayer
       if (has_layers && length(dim(net[[1]])) == 3) {
           # multilayer longitudinal list - extract layers from first time period
           layer_names <- attrs$layers
           n_layers <- length(layer_names)
           
           # extract each layer across all time periods
           layer_list <- vector("list", n_layers)
           names(layer_list) <- layer_names
           
           for (l in seq_len(n_layers)) {
               # create longitudinal list for this layer
               time_list <- vector("list", length(net))
               names(time_list) <- names(net)
               
               for (t in seq_along(net)) {
                   # extract layer l from time t
                   layer_slice <- net[[t]][,,l]
                   
                   # preserve attributes
                   attr(layer_slice, "netify_type") <- "cross_sec"
                   attr(layer_slice, "symmetric") <- attrs$symmetric
                   attr(layer_slice, "mode") <- attrs$mode
                   attr(layer_slice, "weight") <- attrs$weight
                   attr(layer_slice, "layer") <- layer_names[l]
                   class(layer_slice) <- "netify"
                   
                   time_list[[t]] <- layer_slice
               }
               
               # wrap as longitudinal list
               attr(time_list, "netify_type") <- "longit_list"
               attr(time_list, "symmetric") <- attrs$symmetric
               attr(time_list, "mode") <- attrs$mode
               attr(time_list, "weight") <- attrs$weight
               attr(time_list, "layer") <- layer_names[l]
               class(time_list) <- "netify"
               
               layer_list[[l]] <- time_list
           }
           
           return(layer_list)
       } else {
           # regular longitudinal list
           return(net)
       }
   } else if (attrs$netify_type == "longit_array") {
       # check if multilayer (4D array)
       if (has_layers && length(dim(net)) == 4) {
           # multilayer longitudinal array [actors × actors × layers × time]
           dims <- dim(net)
           layer_names <- attrs$layers
           time_names <- dimnames(net)[[4]]
           if (is.null(time_names)) {
               time_names <- as.character(1:dims[4])
           }
           
           # extract each layer
           layer_list <- vector("list", dims[3])
           names(layer_list) <- layer_names
           
           for (l in seq_len(dims[3])) {
               # extract 3D array for this layer [actors × actors × time]
               layer_array <- net[,,l,]
               
               # set proper dimensions
               dim(layer_array) <- c(dims[1], dims[2], dims[4])
               dimnames(layer_array) <- list(
                   dimnames(net)[[1]], 
                   dimnames(net)[[2]], 
                   time_names
               )
               
               # preserve attributes
               attr(layer_array, "netify_type") <- "longit_array"
               attr(layer_array, "symmetric") <- attrs$symmetric
               attr(layer_array, "mode") <- attrs$mode
               attr(layer_array, "weight") <- attrs$weight
               attr(layer_array, "layer") <- layer_names[l]
               class(layer_array) <- "netify"
               
               layer_list[[l]] <- layer_array
           }
           
           return(layer_list)
       } else {
           # regular longitudinal array (3D)
           dims <- dim(net)
           time_names <- dimnames(net)[[3]]
           if (is.null(time_names)) {
               time_names <- as.character(1:dims[3])
           }
       
       # check sparsity
       total_elements <- prod(dims)
       non_zero <- sum(net != 0, na.rm = TRUE)
       sparsity <- non_zero / total_elements
       
       # use sparse operations if very sparse
       if (sparsity < 0.1 && dims[1] > 100) {
           # for very sparse large networks, use sparse melting
           sparse_data <- melt_array_sparse(net)
           
           # reconstruct list from sparse data
           net_list <- lapply(time_names, function(t) {
               slice_data <- sparse_data[sparse_data$L1 == t, ]
               if (nrow(slice_data) == 0) {
                   mat <- matrix(0, dims[1], dims[2], 
                               dimnames = list(dimnames(net)[[1]], dimnames(net)[[2]]))
               } else {
                   mat <- matrix(0, dims[1], dims[2], 
                               dimnames = list(dimnames(net)[[1]], dimnames(net)[[2]]))
                   for (i in 1:nrow(slice_data)) {
                       mat[slice_data$Var1[i], slice_data$Var2[i]] <- slice_data$value[i]
                   }
               }
               # preserve attributes
               attr(mat, "netify_type") <- "cross_sec"
               attr(mat, "symmetric") <- attrs$symmetric
               attr(mat, "mode") <- attrs$mode
               attr(mat, "weight") <- attrs$weight
               # don't preserve layers attribute for non-multilayer
               class(mat) <- "netify"
               mat
           })
       } else {
           # regular approach for denser networks
           net_list <- vector("list", dims[3])
           for (t in 1:dims[3]) {
               # extract time slice and preserve as matrix
               time_slice <- net[,,t]
               # preserve attributes from original
               attr(time_slice, "netify_type") <- "cross_sec"
               attr(time_slice, "symmetric") <- attrs$symmetric
               attr(time_slice, "mode") <- attrs$mode
               attr(time_slice, "weight") <- attrs$weight
               # don't preserve layers attribute for non-multilayer
               class(time_slice) <- "netify"
               
               net_list[[t]] <- time_slice
           }
       }
       
       names(net_list) <- time_names
       return(net_list)
       }
   } else if (attrs$netify_type == "cross_sec") {
       # check if multilayer (3D array)
       if (has_layers && length(dim(net)) == 3) {
           # multilayer cross-sectional [actors × actors × layers]
           dims <- dim(net)
           layer_names <- attrs$layers
           
           # extract each layer
           layer_list <- vector("list", dims[3])
           names(layer_list) <- layer_names
           
           for (l in seq_len(dims[3])) {
               # extract layer slice
               layer_slice <- net[,,l]
               
               # preserve attributes
               attr(layer_slice, "netify_type") <- "cross_sec"
               attr(layer_slice, "symmetric") <- attrs$symmetric
               attr(layer_slice, "mode") <- attrs$mode
               attr(layer_slice, "weight") <- attrs$weight
               attr(layer_slice, "layer") <- layer_names[l]
               class(layer_slice) <- "netify"
               
               layer_list[[l]] <- layer_slice
           }
           
           return(layer_list)
       } else {
           # single cross-sectional network
           return(list(network = net))
       }
   } else {
       # unknown type - return as single network
       return(list(network = net))
   }
}

#' Prepare Networks for By-Group Comparison
#'
#' `prepare_by_group_networks` subsets a network based on nodal attributes to create separate networks for each group value (currently not fully implemented).
#'
#' @param net A netify object containing nodal attributes.
#' @param by Character string specifying the nodal attribute to group by.
#'
#' @return A list of netify objects, one for each group (currently returns original network).
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @keywords internal
#' @noRd

prepare_by_group_networks <- function(net, by){

   # extract attributes
   attrs <- attributes(net)
   nodal_data <- attrs$nodal_data
   
   if (is.null(nodal_data) || !by %in% names(nodal_data)) {
       cli::cli_abort(
           c("x" = "Attribute '{by}' not found in nodal data",
             "i" = "Available attributes: {paste(names(nodal_data), collapse = ', ')}",
             "!" = "Please specify a valid attribute name from nodal_data")
       )
   }
   
   # get unique groups
   groups <- unique(nodal_data[[by]])
   groups <- groups[!is.na(groups)]
   
   # create subnetworks for each group
   nets_list <- list()
   
   # this is a simplified version - might need to implement proper subsetting
   cli::cli_alert_warning("By-group comparison is not fully implemented yet")
   
   # for now, just return the original network
   return(list(net))
}

#' Analyze Network Comparisons by Group
#'
#' `analyze_by_group` performs additional analysis on comparison results when networks are grouped by nodal attributes (currently not fully implemented).
#'
#' @param results A netify_comparison object from compare_networks.
#' @param nets_list The original list of networks being compared.
#' @param by Character string specifying the grouping attribute.
#'
#' @return A list containing by-group analysis results (currently returns placeholder).
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @keywords internal
#' @noRd

analyze_by_group <- function(results, nets_list, by){

   # extract similarity matrix from results
   if (!is.null(results$details) && !is.null(results$details$correlation_matrix)) {
       sim_mat <- results$details$correlation_matrix
   } else {
       cli::cli_alert_warning("Need return_details=TRUE for by-group analysis")
       return(NULL)
   }
   
   # simplified version for now
   return(list(
       message = "By-group analysis not fully implemented"
   ))
}

#' Extract Matrix from Netify Object
#'
#' `extract_matrix` extracts the adjacency matrix from different types of netify objects, handling cross-sectional, longitudinal array, and longitudinal list formats.
#'
#' @param net A netify object of any type.
#'
#' @return A matrix representing the network adjacency matrix.
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @keywords internal
#' @noRd

extract_matrix <- function(net){

   #
   attrs <- attributes(net)
   
   if (attrs$netify_type == "cross_sec") {
       # remove class to get plain matrix
       mat <- unclass(net)
       attributes(mat) <- attributes(mat)[c("dim", "dimnames")]
       return(mat)
   } else if (attrs$netify_type == "longit_list") {
       # return first time period with warning
       cli::cli_alert_warning("Using first time period for comparison. Consider subsetting first.")
       mat <- unclass(net[[1]])
       attributes(mat) <- attributes(mat)[c("dim", "dimnames")]
       return(mat)
   } else if (attrs$netify_type == "longit_array") {
       # return first time period
       cli::cli_alert_warning("Using first time period for comparison. Consider subsetting first.")
       return(net[,,1])
   }
}

#' Align Network Matrices for Comparison
#'
#' `align_matrices` ensures two network matrices have the same dimensions and actor ordering by creating aligned versions with all actors from both networks.
#'
#' @param net1 First netify object to align.
#' @param net2 Second netify object to align.
#' @param include_diagonal Logical; whether to preserve diagonal values.
#'
#' @return A list containing two aligned matrices with same dimensions and actor ordering.
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @keywords internal
#' @noRd

align_matrices <- function(net1, net2, include_diagonal = FALSE) {
   # extract matrices
   mat1 <- extract_matrix(net1)
   mat2 <- extract_matrix(net2)
   
   # get all actors more efficiently
   all_actors <- sort(unique(c(
       rownames(mat1), colnames(mat1),
       rownames(mat2), colnames(mat2)
   )))
   
   n <- length(all_actors)
   aligned1 <- aligned2 <- matrix(0, n, n, dimnames = list(all_actors, all_actors))
   
   # vectorized indexing for efficiency
   idx1_rows <- match(rownames(mat1), all_actors)
   idx1_cols <- match(colnames(mat1), all_actors)
   aligned1[idx1_rows, idx1_cols] <- mat1
   
   idx2_rows <- match(rownames(mat2), all_actors)
   idx2_cols <- match(colnames(mat2), all_actors)
   aligned2[idx2_rows, idx2_cols] <- mat2
   
   if (!include_diagonal) {
       diag(aligned1) <- diag(aligned2) <- NA
   }
   
   return(list(mat1 = aligned1, mat2 = aligned2))
}

#' Align Single Network to Actor Set
#'
#' `align_to_actors` aligns a single network to a pre-specified set of actors.
#'
#' @param net A netify object to align.
#' @param all_actors Character vector of all actors to align to.
#' @param include_diagonal Logical; whether to preserve diagonal values.
#'
#' @return An aligned matrix with dimensions matching all_actors.
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @keywords internal
#' @noRd

align_to_actors <- function(net, all_actors, include_diagonal = FALSE) {
   mat <- extract_matrix(net)
   
   n <- length(all_actors)
   aligned <- matrix(0, n, n, dimnames = list(all_actors, all_actors))
   
   # vectorized indexing
   idx_rows <- match(rownames(mat), all_actors)
   idx_cols <- match(colnames(mat), all_actors)
   idx_rows <- idx_rows[!is.na(idx_rows)]
   idx_cols <- idx_cols[!is.na(idx_cols)]
   
   if (length(idx_rows) > 0 && length(idx_cols) > 0) {
       aligned[idx_rows, idx_cols] <- mat[rownames(mat) %in% all_actors[idx_rows], 
                                         colnames(mat) %in% all_actors[idx_cols]]
   }
   
   if (!include_diagonal) {
       diag(aligned) <- NA
   }
   
   return(aligned)
}

#' Calculate Jaccard Similarity Between Networks (Fast Version)
#'
#' `calculate_jaccard_fast` computes the Jaccard similarity coefficient between two networks after binarizing based on specified thresholds.
#'
#' @param mat1 First network matrix.
#' @param mat2 Second network matrix.
#' @param threshold1 Threshold for binarizing first matrix.
#' @param threshold2 Threshold for binarizing second matrix.
#'
#' @return Numeric Jaccard similarity coefficient between 0 and 1.
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @keywords internal
#' @noRd

calculate_jaccard_fast <- function(mat1, mat2, threshold1, threshold2){

    # binarize matrices based on thresholds
    bin1 <- (mat1 > threshold1) & !is.na(mat1)
    bin2 <- (mat2 > threshold2) & !is.na(mat2)
    
    # vectorized calculation
    intersection <- sum(bin1 & bin2, na.rm = TRUE)
    union <- sum(bin1 | bin2, na.rm = TRUE)
    
    # handle edge cases
    if (union == 0) {
        # both networks are empty - jaccard is considered 0 (no similarity)
        return(0)
    }
    return(intersection / union)
}

#' Calculate Hamming Distance Between Networks (Fast Version)
#'
#' `calculate_hamming_fast` computes the normalized Hamming distance (proportion of differing edges) between two networks after binarizing.
#'
#' @param mat1 First network matrix.
#' @param mat2 Second network matrix.
#' @param threshold1 Threshold for binarizing first matrix.
#' @param threshold2 Threshold for binarizing second matrix.
#'
#' @return Numeric Hamming distance between 0 and 1.
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @keywords internal
#' @noRd

calculate_hamming_fast <- function(mat1, mat2, threshold1, threshold2){

   # binarize
   bin1 <- (mat1 > threshold1) & !is.na(mat1)
   bin2 <- (mat2 > threshold2) & !is.na(mat2)
   
   # vectorized calculation
   valid <- !is.na(bin1) & !is.na(bin2)
   total <- sum(valid)
   if (total == 0) return(NA)
   
   different <- sum(bin1[valid] != bin2[valid])
   return(different / total)
}

#' Perform QAP Correlation Test (Fast Version)
#'
#' `qap_correlation_fast` calculates the correlation between two networks and tests its significance using the Quadratic Assignment Procedure with permutation testing.
#'
#' @param mat1 First network matrix.
#' @param mat2 Second network matrix.
#' @param n_permutations Integer; number of permutations for significance testing.
#'
#' @return A list containing the observed correlation and p-value from permutation test.
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @keywords internal
#' @noRd

qap_correlation_fast <- function(mat1, mat2, n_permutations){

   # calc observed correlation using cpp function
   vec1 <- as.vector(mat1)
   vec2 <- as.vector(mat2)
   complete <- !is.na(vec1) & !is.na(vec2)
   
   if (sum(complete) < 3) {
       return(list(correlation = NA, p_value = NA))
   }
   
   obs_cor <- correlation_cpp(vec1[complete], vec2[complete])
   
   # vectorized permutation test
   n <- nrow(mat1)
   
   # create permutation matrix all at once for efficiency
   perm_matrix <- replicate(n_permutations, sample(n))
   
   # vectorized correlation calculation
   perm_cors <- apply(perm_matrix, 2, function(perm) {
       mat1_perm <- mat1[perm, perm]
       vec1_perm <- as.vector(mat1_perm)
       complete_perm <- !is.na(vec1_perm) & !is.na(vec2)
       if (sum(complete_perm) >= 3) {
           correlation_cpp(vec1_perm[complete_perm], vec2[complete_perm])
       } else {
           NA
       }
   })
   
   # calc p-value
   valid_perms <- !is.na(perm_cors)
   p_value <- if(sum(valid_perms) > 0) {
       mean(abs(perm_cors[valid_perms]) >= abs(obs_cor))
   } else {
       NA
   }
   
   return(list(correlation = obs_cor, p_value = p_value))
}

#' Calculate Edge Changes Between Networks (Fast Version)
#'
#' `calculate_edge_changes_fast` identifies edges that are added, removed, or maintained between two networks and calculates weight correlation for maintained edges.
#'
#' @param mat1 First network matrix.
#' @param mat2 Second network matrix.
#' @param threshold1 Threshold for determining edge presence in first matrix.
#' @param threshold2 Threshold for determining edge presence in second matrix.
#'
#' @return A list containing counts of added, removed, and maintained edges plus weight correlation.
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @keywords internal
#' @noRd

calculate_edge_changes_fast <- function(mat1, mat2, threshold1, threshold2){

   # binarize
   bin1 <- (mat1 > threshold1) & !is.na(mat1)
   bin2 <- (mat2 > threshold2) & !is.na(mat2)
   
   # calc changes - vectorized
   added <- sum(bin2 & !bin1, na.rm = TRUE)
   removed <- sum(bin1 & !bin2, na.rm = TRUE)
   maintained <- sum(bin1 & bin2, na.rm = TRUE)
   
   # weight correlation for maintained edges using cpp function
   maintained_edges <- bin1 & bin2
   if (sum(maintained_edges, na.rm = TRUE) > 0) {
       vals1 <- mat1[maintained_edges]
       vals2 <- mat2[maintained_edges]
       complete <- !is.na(vals1) & !is.na(vals2)
       if (sum(complete) > 0) {
           weight_cor <- correlation_cpp(vals1[complete], vals2[complete])
       } else {
           weight_cor <- NA
       }
   } else {
       weight_cor <- NA
   }
   
   return(list(
       added = added,
       removed = removed,
       maintained = maintained,
       weight_correlation = weight_cor
   ))
}

#' Create Edge Comparison Summary
#'
#' `create_edge_summary` formats the results of edge comparisons into a summary data frame, handling both pairwise and multiple network comparisons.
#'
#' @param correlation_mat Matrix of correlation coefficients between networks.
#' @param jaccard_mat Matrix of Jaccard similarities between networks.
#' @param hamming_mat Matrix of Hamming distances between networks.
#' @param qap_mat Matrix of QAP correlations between networks.
#' @param qap_pval_mat Matrix of QAP p-values between networks.
#' @param method Character string specifying which metrics to include in summary.
#'
#' @return A data frame summarizing the comparison results.
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @keywords internal
#' @noRd

create_edge_summary <- function(
    correlation_mat, jaccard_mat, hamming_mat, 
    qap_mat, qap_pval_mat, spectral_mat, method){
    
    # create summary dataframe based on number of networks
    n <- nrow(correlation_mat)
    if (n == 2) {
        # simple two-network comparison
        summary_df <- data.frame(
            comparison = paste(rownames(correlation_mat)[1], "vs", 
                             rownames(correlation_mat)[2]),
            stringsAsFactors = FALSE
        )
        
        if (method %in% c("correlation", "all")) {
            summary_df$correlation <- correlation_mat[1,2]
        }
        if (method %in% c("jaccard", "all")) {
            summary_df$jaccard <- jaccard_mat[1,2]
        }
        if (method %in% c("hamming", "all")) {
            summary_df$hamming <- hamming_mat[1,2]
        }
        if (method %in% c("qap", "all") && !is.na(qap_mat[1,2])) {
            summary_df$qap_correlation <- qap_mat[1,2]
            summary_df$qap_pvalue <- qap_pval_mat[1,2]
        }
        if (method %in% c("spectral", "all")) {
            summary_df$spectral <- spectral_mat[1,2]
        }
    } else {
       # multiple network comparison - return average similarities
       summary_df <- data.frame(
           metric = character(),
           mean = numeric(),
           sd = numeric(),
           min = numeric(),
           max = numeric(),
           stringsAsFactors = FALSE
       )
       
       if (method %in% c("correlation", "all")) {
           cors <- correlation_mat[lower.tri(correlation_mat)]
           summary_df <- rbind(summary_df, data.frame(
               metric = "correlation",
               mean = mean(cors, na.rm = TRUE),
               sd = sd(cors, na.rm = TRUE),
               min = min(cors, na.rm = TRUE),
               max = max(cors, na.rm = TRUE),
               stringsAsFactors = FALSE
           ))
       }
       
       if (method %in% c("jaccard", "all")) {
           jacs <- jaccard_mat[lower.tri(jaccard_mat)]
           summary_df <- rbind(summary_df, data.frame(
               metric = "jaccard",
               mean = mean(jacs, na.rm = TRUE),
               sd = sd(jacs, na.rm = TRUE),
               min = min(jacs, na.rm = TRUE),
               max = max(jacs, na.rm = TRUE),
               stringsAsFactors = FALSE
           ))
       }
       
       if (method %in% c("hamming", "all")) {
           hams <- hamming_mat[lower.tri(hamming_mat)]
           summary_df <- rbind(summary_df, data.frame(
               metric = "hamming",
               mean = mean(hams, na.rm = TRUE),
               sd = sd(hams, na.rm = TRUE),
               min = min(hams, na.rm = TRUE),
               max = max(hams, na.rm = TRUE),
               stringsAsFactors = FALSE
           ))
       }
       
       if (method %in% c("spectral", "all")) {
           specs <- spectral_mat[lower.tri(spectral_mat)]
           summary_df <- rbind(summary_df, data.frame(
               metric = "spectral",
               mean = mean(specs, na.rm = TRUE),
               sd = sd(specs, na.rm = TRUE),
               min = min(specs, na.rm = TRUE),
               max = max(specs, na.rm = TRUE),
               stringsAsFactors = FALSE
           ))
       }
   }
   
   return(summary_df)
}

#' Compare Single Attribute Between Networks
#'
#' `compare_single_attribute` compares the distribution of a specific nodal attribute across networks using appropriate similarity measures and statistical tests.
#'
#' @param nets_list A list of netify objects to compare.
#' @param attrs_list List of nodal attribute data frames from each network.
#' @param attribute Character string specifying which attribute to compare.
#' @param test Logical; whether to perform statistical testing.
#' @param n_permutations Integer; number of permutations (currently unused).
#'
#' @return A list containing similarity comparisons and optional test results.
#'
#' @author Cassy Dorff, Shahryar Minhas
#' 
#' @importFrom stats ks.test
#'
#' @keywords internal
#' @noRd

compare_single_attribute <- function(
   nets_list, attrs_list, attribute, 
   test = TRUE, n_permutations = 1000){
  
   n_nets <- length(nets_list)
   net_names <- names(nets_list)
   if (is.null(net_names)) {
       net_names <- paste0("net", seq_along(nets_list))
   }
   
   # extract attribute values
   attr_values_list <- list()
   
   for (i in 1:n_nets) {
       if (!is.null(attrs_list[[i]]) && attribute %in% names(attrs_list[[i]])) {
           # get attribute values
           attr_vals <- attrs_list[[i]][[attribute]]
           actors <- attrs_list[[i]]$actor
           
           # remove missing values
           complete <- !is.na(attr_vals)
           attr_vals <- attr_vals[complete]
           actors <- actors[complete]
           
           attr_values_list[[i]] <- list(values = attr_vals, actors = actors)
       }
   }
   
   # compare attribute distributions pairwise
   comparison_mat <- matrix(NA, n_nets, n_nets, 
                          dimnames = list(net_names, net_names))
   ks_test_mat <- matrix(NA, n_nets, n_nets,
                        dimnames = list(net_names, net_names))
   
   for (i in 1:(n_nets-1)) {
       for (j in (i+1):n_nets) {
           if (!is.null(attr_values_list[[i]]) && !is.null(attr_values_list[[j]])) {
               vals1 <- attr_values_list[[i]]$values
               vals2 <- attr_values_list[[j]]$values
               
               if (is.numeric(vals1) && is.numeric(vals2)) {
                   # for numeric attributes, use KS test
                   if (test && length(vals1) > 1 && length(vals2) > 1) {
                       ks_result <- ks.test(vals1, vals2)
                       ks_test_mat[i,j] <- ks_test_mat[j,i] <- ks_result$p.value
                   }
                   
                   # compare distributions
                   comparison_mat[i,j] <- comparison_mat[j,i] <- 
                       compare_distributions(vals1, vals2)
                   
               } else {
                   # for categorical, compare frequency distributions
                   comparison_mat[i,j] <- comparison_mat[j,i] <- 
                       compare_categorical_distributions(vals1, vals2)
               }
           }
       }
   }
   
   # set diagonal
   diag(comparison_mat) <- 1
   if (test) diag(ks_test_mat) <- 1
   
   # create summary
   if (n_nets == 2) {
       summary <- data.frame(
           comparison = paste(net_names[1], "vs", net_names[2]),
           similarity = comparison_mat[1,2],
           stringsAsFactors = FALSE
       )
       if (test && !is.na(ks_test_mat[1,2])) {
           summary$ks_pvalue <- ks_test_mat[1,2]
       }
   } else {
       # average similarities
       summary <- data.frame(
           mean_similarity = mean(comparison_mat[lower.tri(comparison_mat)], 
                                na.rm = TRUE),
           sd_similarity = sd(comparison_mat[lower.tri(comparison_mat)], 
                             na.rm = TRUE),
           stringsAsFactors = FALSE
       )
   }
   
   return(list(
       summary = summary,
       details = list(
           similarity_matrix = comparison_mat,
           ks_test_matrix = if(test) ks_test_mat else NULL,
           attribute_values = attr_values_list
       )
   ))
}

#' Compare Continuous Distributions
#'
#' `compare_distributions` calculates similarity between two continuous distributions by correlating their empirical cumulative distribution functions.
#'
#' @param vals1 Numeric vector of values from first distribution.
#' @param vals2 Numeric vector of values from second distribution.
#'
#' @return Numeric correlation coefficient between the two ECDFs.
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @importFrom stats ecdf cor
#' 
#' @keywords internal
#' @noRd

compare_distributions <- function(vals1, vals2) {
    # simple approach: correlation of empirical CDFs
    
    # check for empty or all-NA values first
    if (length(vals1) == 0 || all(is.na(vals1)) || 
        length(vals2) == 0 || all(is.na(vals2))) {
        return(NA)
    }
    
    # remove NAs
    vals1 <- vals1[!is.na(vals1)]
    vals2 <- vals2[!is.na(vals2)]
    
    # check again after removing NAs
    if (length(vals1) == 0 || length(vals2) == 0) {
        return(NA)
    }
    
    # create empirical CDFs
    all_vals <- sort(unique(c(vals1, vals2)))
    
    # handle case where all values are identical
    if (length(all_vals) == 1) {
        return(1)  # perfect similarity if all values are the same
    }
    
    ecdf1 <- ecdf(vals1)
    ecdf2 <- ecdf(vals2)
    
    # evaluate at common points
    cdf1 <- ecdf1(all_vals)
    cdf2 <- ecdf2(all_vals)
    
    # return correlation of CDFs
    if (length(all_vals) > 1) {
        return(cor(cdf1, cdf2))
    } else {
        return(NA)
    }
}

#' Compare Categorical Distributions
#'
#' `compare_categorical_distributions` calculates similarity between two categorical distributions using total variation distance.
#'
#' @param vals1 Vector of categorical values from first distribution.
#' @param vals2 Vector of categorical values from second distribution.
#'
#' @return Numeric similarity score (1 - total variation distance).
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @keywords internal
#' @noRd

compare_categorical_distributions <- function(vals1, vals2) {
   # get frequency tables
   tab1 <- table(vals1)
   tab2 <- table(vals2)
   
   # get all categories
   all_cats <- sort(unique(c(names(tab1), names(tab2))))
   
   # create aligned frequency vectors
   freq1 <- numeric(length(all_cats))
   freq2 <- numeric(length(all_cats))
   
   names(freq1) <- names(freq2) <- all_cats
   
   freq1[names(tab1)] <- tab1
   freq2[names(tab2)] <- tab2
   
   # normalize to proportions
   prop1 <- freq1 / sum(freq1)
   prop2 <- freq2 / sum(freq2)
   
   # calc similarity (1 - total variation distance)
   tv_distance <- 0.5 * sum(abs(prop1 - prop2))
   similarity <- 1 - tv_distance
   
   return(similarity)
}

#' Print method for netify_comparison objects
#'
#' @param x A netify_comparison object
#' @param ... Additional arguments (not used)
#' 
#' @export

print.netify_comparison <- function(x, ...) {
   cat("Network Comparison Results\n")
   cat("==========================\n")
   cat("Type:", x$comparison_type, "\n")
   cat("Method:", x$method, "\n")
   cat("Networks compared:", x$n_networks, "\n\n")
   
   if (!is.null(x$summary)) {
       cat("Summary Statistics:\n")
       print(x$summary, row.names = FALSE)
   }
   
   if (!is.null(x$edge_changes) && length(x$edge_changes) > 0) {
       cat("\nEdge Changes:\n")
       for (comp in names(x$edge_changes)) {
           changes <- x$edge_changes[[comp]]
           cat(sprintf("  %s: %d added, %d removed, %d maintained\n",
                      comp, changes$added, changes$removed, changes$maintained))
       }
   }
   
   if (!is.null(x$node_changes) && length(x$node_changes) > 0) {
       cat("\nNode Changes:\n")
       for (comp in names(x$node_changes)) {
           changes <- x$node_changes[[comp]]
           cat(sprintf("  %s: %d added, %d removed, %d maintained\n",
                      comp, changes$n_added, changes$n_removed, changes$n_maintained))
       }
   }
   
   if (!is.null(x$by_attribute)) {
       cat("\nAttribute Comparisons:\n")
       attrs <- names(x$by_attribute)
       cat("  Attributes analyzed:", paste(attrs, collapse = ", "), "\n")
   }
   
   invisible(x)
}

#' Calculate Spectral Distance Between Networks
#'
#' Computes the spectral distance between two networks based on their eigenvalue
#' spectra. The spectral distance quantifies how different two graphs are by
#' comparing their eigenvalues.
#'
#' @param mat1 First adjacency matrix
#' @param mat2 Second adjacency matrix  
#' @param laplacian Logical; whether to use Laplacian eigenvalues (default TRUE)
#'   instead of adjacency matrix eigenvalues
#'
#' @return Numeric spectral distance between 0 and sqrt(2*n) where n is the
#'   number of nodes. Lower values indicate more similar networks.
#'
#' @details
#' The spectral distance is calculated as:
#' \deqn{d_{spectral}(G_1, G_2) = \sqrt{\sum_{i=1}^n (\lambda_i^{(1)} - \lambda_i^{(2)})^2}}
#' 
#' where \eqn{\lambda_i^{(1)}} and \eqn{\lambda_i^{(2)}} are the sorted eigenvalues
#' of the two networks' Laplacian (or adjacency) matrices.
#' 
#' For directed networks, we symmetrize by averaging with the transpose.
#' Missing values are replaced with zeros.
#'
#' @author Shahryar Minhas
#'
#' @keywords internal
#' @noRd
calculate_spectral_distance <- function(mat1, mat2, laplacian = TRUE) {
    # handle missing values
    mat1[is.na(mat1)] <- 0
    mat2[is.na(mat2)] <- 0
    
    # ensure matrices are same size
    n1 <- nrow(mat1)
    n2 <- nrow(mat2)
    
    if (n1 != n2) {
        # pad smaller matrix with zeros
        n <- max(n1, n2)
        if (n1 < n) {
            mat1_new <- matrix(0, n, n)
            mat1_new[1:n1, 1:n1] <- mat1
            mat1 <- mat1_new
        }
        if (n2 < n) {
            mat2_new <- matrix(0, n, n)
            mat2_new[1:n2, 1:n2] <- mat2
            mat2 <- mat2_new
        }
    }
    
    # symmetrize if not symmetric (for directed networks)
    if (!isSymmetric(mat1)) {
        mat1 <- (mat1 + t(mat1)) / 2
    }
    if (!isSymmetric(mat2)) {
        mat2 <- (mat2 + t(mat2)) / 2
    }
    
    # calculate eigenvalues
    if (laplacian) {
        # compute laplacian matrices
        deg1 <- rowSums(mat1)
        deg2 <- rowSums(mat2)
        L1 <- diag(deg1) - mat1
        L2 <- diag(deg2) - mat2
        
        # get eigenvalues
        eigen1 <- eigen(L1, symmetric = TRUE, only.values = TRUE)$values
        eigen2 <- eigen(L2, symmetric = TRUE, only.values = TRUE)$values
    } else {
        # use adjacency matrix eigenvalues
        eigen1 <- eigen(mat1, symmetric = TRUE, only.values = TRUE)$values
        eigen2 <- eigen(mat2, symmetric = TRUE, only.values = TRUE)$values
    }
    
    # sort eigenvalues in decreasing order
    eigen1 <- sort(eigen1, decreasing = TRUE)
    eigen2 <- sort(eigen2, decreasing = TRUE)
    
    # calculate spectral distance
    spectral_dist <- sqrt(sum((eigen1 - eigen2)^2))
    
    return(spectral_dist)
}