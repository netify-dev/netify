#' Create ego-centric layouts for ego networks
#'
#' These functions create specialized layouts for ego networks that place the ego
#' at the center and arrange alters in meaningful ways around it.
#'
#' @param netlet A netify object created with ego_netify()
#' @param min_radius Minimum distance from ego to any alter
#' @param max_radius Maximum distance from ego to any alter
#' @param seed Random seed for reproducible layouts
#' @param buffer_radius Minimum distance from ego for ego_centric layout
#' @param transition_zone Smooth transition zone width for ego_centric layout
#' @param n_rings Number of concentric rings for radial layout
#' @param ego_name Name of the ego node (extracted from netlet attributes if not provided)
#'
#' @return A list of data frames with x,y coordinates for each time period
#'
#' @details
#' Three layout algorithms are provided:
#' 
#' \strong{Hierarchical Layout}: Places ego at center and arranges alters in 
#' concentric circles based on their network centrality. More central alters 
#' are placed closer to the ego.
#' 
#' \strong{Radial Layout}: Places ego at center and arranges alters in 
#' concentric rings based on connection strength quartiles. Stronger connections
#' are placed in inner rings.
#' 
#' \strong{Ego Centric Layout}: Uses force-directed layout but ensures ego 
#' remains at center with a buffer zone. Preserves natural clustering while
#' maintaining ego visibility.
#'
#' @name ego_layouts
#' @examples
#' \dontrun{
#' # Create ego network
#' ego_net <- ego_netify(my_network, ego = "Pakistan")
#' 
#' # Hierarchical layout
#' layout <- create_hierarchical_ego_layout(ego_net)
#' plot(ego_net, point_layout = layout)
#' 
#' # Radial layout with custom rings
#' layout <- create_radial_ego_layout(ego_net, n_rings = 5)
#' plot(ego_net, point_layout = layout)
#' }
NULL

#' @rdname ego_layouts
#' @export
create_hierarchical_ego_layout <- function(netlet, min_radius = 2, max_radius = 5, seed = 123) {
    # Get ego name
    ego_name <- attr(netlet, "ego_id")
    if(is.null(ego_name)) {
        stop("Not an ego network - ego_id attribute not found")
    }
    
    # Get actor summary to use centrality measures
    actor_summary <- summary_actor(netlet)
    
    # Get base layout structure
    base_layout <- get_node_layout(netlet, layout = "circle", seed = seed)
    
    # Process each time period
    hierarchical_layouts <- lapply(seq_along(base_layout), function(t_idx) {
        layout_df <- base_layout[[t_idx]]
        
        if(is.null(layout_df) || nrow(layout_df) == 0) {
            return(layout_df)
        }
        
        # Get the time period name
        time_name <- names(base_layout)[t_idx]
        
        # Get centrality data for this time period
        if("time" %in% names(actor_summary)) {
            centrality_t <- actor_summary[actor_summary$time == time_name, ]
        } else {
            centrality_t <- actor_summary
        }
        
        # Find ego
        ego_idx <- which(layout_df$actor == ego_name)
        
        if(length(ego_idx) == 0) {
            return(layout_df)
        }
        
        # Place ego at center
        layout_df$x[ego_idx] <- 0
        layout_df$y[ego_idx] <- 0
        
        # Get alters
        alter_idx <- setdiff(1:nrow(layout_df), ego_idx)
        n_alters <- length(alter_idx)
        
        if(n_alters > 0) {
            # Get centrality scores for alters (use total_degree for directed networks)
            alter_names <- layout_df$actor[alter_idx]
            alter_centrality <- numeric(n_alters)
            
            for(i in 1:n_alters) {
                actor_row <- centrality_t[centrality_t$actor == alter_names[i], ]
                if(nrow(actor_row) > 0) {
                    # Use appropriate centrality measure
                    if("total_degree" %in% names(actor_row)) {
                        alter_centrality[i] <- actor_row$total_degree
                    } else if("degree" %in% names(actor_row)) {
                        alter_centrality[i] <- actor_row$degree
                    } else {
                        alter_centrality[i] <- 1  # Default if no degree found
                    }
                } else {
                    alter_centrality[i] <- 1
                }
            }
            
            # Rank nodes by centrality (higher centrality = closer to ego)
            ranks <- rank(-alter_centrality, ties.method = "random")
            
            # Map ranks to radii
            radii <- min_radius + (ranks - 1) / max(ranks - 1, 1) * (max_radius - min_radius)
            
            # Distribute angles evenly
            angles <- seq(0, 2 * pi * (n_alters - 1) / n_alters, length.out = n_alters)
            
            # Add some variation to avoid perfect circles
            angles <- angles + runif(n_alters, -pi/(4*n_alters), pi/(4*n_alters))
            
            # Assign positions
            for(i in 1:n_alters) {
                idx <- alter_idx[i]
                layout_df$x[idx] <- radii[i] * cos(angles[i])
                layout_df$y[idx] <- radii[i] * sin(angles[i])
            }
        }
        
        return(layout_df)
    })
    
    names(hierarchical_layouts) <- names(base_layout)
    return(hierarchical_layouts)
}

#' @rdname ego_layouts
#' @export
create_radial_ego_layout <- function(netlet, ego_name = NULL, n_rings = 4, 
                                   min_radius = 1.5, max_radius = 5, seed = 123) {
    # Get ego name from attributes if not specified
    if(is.null(ego_name)) {
        ego_name <- attr(netlet, "ego_id")
    }
    if(is.null(ego_name)) {
        stop("Not an ego network - ego_id attribute not found")
    }
    
    # Get node layout structure
    default_layout <- get_node_layout(netlet, layout = "fr", seed = seed)
    
    # Process each time period
    radial_layout_list <- lapply(seq_along(default_layout), function(t_idx) {
        layout_df <- default_layout[[t_idx]]
        t_name <- names(default_layout)[t_idx]
        
        # Find ego position
        ego_idx <- which(layout_df$actor == ego_name)
        
        if(length(ego_idx) == 0) {
            return(layout_df)
        }
        
        # Center ego at origin
        layout_df$x[ego_idx] <- 0
        layout_df$y[ego_idx] <- 0
        
        # Get the network matrix for this time period
        if(attr(netlet, "netify_type") == "longit_list") {
            net_matrix <- netlet[[t_idx]]
        } else {
            net_matrix <- netlet
        }
        
        # Calculate connection strengths to ego
        alter_idx <- setdiff(1:nrow(layout_df), ego_idx)
        alter_names <- layout_df$actor[alter_idx]
        
        # Get weights for connections to/from ego directly from matrix
        connection_strengths <- numeric(length(alter_names))
        for(i in seq_along(alter_names)) {
            alter <- alter_names[i]
            # Sum of incoming and outgoing connections with ego
            out_weight <- net_matrix[ego_name, alter]
            in_weight <- net_matrix[alter, ego_name]
            
            # Handle NA values
            if(is.na(out_weight)) out_weight <- 0
            if(is.na(in_weight)) in_weight <- 0
            
            connection_strengths[i] <- out_weight + in_weight
        }
        
        # Create rings based on connection strength quantiles
        if(max(connection_strengths) > 0 && length(unique(connection_strengths)) > 1) {
            # Use quantiles to create rings
            strength_quantiles <- quantile(connection_strengths[connection_strengths > 0], 
                                         probs = seq(0, 1, length.out = n_rings + 1))
            
            # Assign each node to a ring
            ring_assignments <- cut(connection_strengths, 
                                   breaks = c(-Inf, unique(strength_quantiles)),
                                   labels = FALSE)
            
            # Calculate radius for each ring
            ring_radii <- seq(min_radius, max_radius, length.out = n_rings)
            
            # For each ring, distribute nodes evenly
            for(ring in 1:n_rings) {
                ring_nodes <- which(ring_assignments == ring)
                n_in_ring <- length(ring_nodes)
                
                if(n_in_ring > 0) {
                    # Calculate angles for this ring
                    # Add small random jitter to prevent perfect alignment across rings
                    start_angle <- runif(1, 0, pi/n_in_ring)
                    angles <- seq(start_angle, start_angle + 2*pi, length.out = n_in_ring + 1)[1:n_in_ring]
                    
                    # Assign positions for nodes in this ring
                    ring_radius <- ring_radii[ring]
                    for(j in seq_along(ring_nodes)) {
                        node_idx <- alter_idx[ring_nodes[j]]
                        angle <- angles[j]
                        
                        # Add small random variation to radius
                        radius_variation <- ring_radius + runif(1, -0.1, 0.1)
                        
                        layout_df$x[node_idx] <- radius_variation * cos(angle)
                        layout_df$y[node_idx] <- radius_variation * sin(angle)
                    }
                }
            }
        } else {
            # If no variation in strengths, place all at middle radius
            n_alters <- length(alter_idx)
            angles <- seq(0, 2 * pi, length.out = n_alters + 1)[1:n_alters]
            mid_radius <- (min_radius + max_radius) / 2
            
            for(i in seq_along(alter_idx)) {
                idx <- alter_idx[i]
                layout_df$x[idx] <- mid_radius * cos(angles[i])
                layout_df$y[idx] <- mid_radius * sin(angles[i])
            }
        }
        
        return(layout_df)
    })
    
    names(radial_layout_list) <- names(default_layout)
    return(radial_layout_list)
}

#' @rdname ego_layouts
#' @export
create_ego_centric_layout <- function(netlet, buffer_radius = 1.5, transition_zone = 0.5, seed = 123) {
    # Get ego name from attributes
    ego_name <- attr(netlet, "ego_id")
    if(is.null(ego_name)) {
        stop("Not an ego network - ego_id attribute not found")
    }
    
    # First get a force-directed layout
    base_layout <- get_node_layout(netlet, layout = "fr", seed = seed)
    
    # Process each time period
    ego_centric_layouts <- lapply(base_layout, function(layout_df) {
        if(is.null(layout_df) || nrow(layout_df) == 0) {
            return(layout_df)
        }
        
        # Find ego
        ego_idx <- which(layout_df$actor == ego_name)
        
        if(length(ego_idx) == 0) {
            return(layout_df)
        }
        
        # Get alter positions
        alter_idx <- setdiff(1:nrow(layout_df), ego_idx)
        
        if(length(alter_idx) > 0) {
            # Center the entire network around (0,0) by shifting based on ego's current position
            shift_x <- layout_df$x[ego_idx]
            shift_y <- layout_df$y[ego_idx]
            
            # Shift all nodes so ego is at origin
            layout_df$x <- layout_df$x - shift_x
            layout_df$y <- layout_df$y - shift_y
            
            # Apply a smooth repulsion from the center
            # This avoids creating an artificial circle
            for(i in alter_idx) {
                x <- layout_df$x[i]
                y <- layout_df$y[i]
                dist_from_center <- sqrt(x^2 + y^2)
                
                if(dist_from_center < 0.1) {
                    # Node is too close to center, place randomly
                    angle <- runif(1, 0, 2*pi)
                    layout_df$x[i] <- buffer_radius * cos(angle)
                    layout_df$y[i] <- buffer_radius * sin(angle)
                } else if(dist_from_center < buffer_radius) {
                    # Apply smooth transition
                    # The closer to center, the stronger the push
                    push_strength <- 1 - (dist_from_center / buffer_radius)
                    push_strength <- push_strength^2  # Quadratic for smoother transition
                    
                    # Calculate unit vector from center
                    unit_x <- x / dist_from_center
                    unit_y <- y / dist_from_center
                    
                    # Push outward proportionally
                    push_distance <- push_strength * (buffer_radius - dist_from_center + transition_zone)
                    layout_df$x[i] <- x + unit_x * push_distance
                    layout_df$y[i] <- y + unit_y * push_distance
                }
                # Nodes already outside buffer zone are left unchanged
            }
            
            # Optional: Scale down if network is too spread out
            all_distances <- sqrt(layout_df$x[alter_idx]^2 + layout_df$y[alter_idx]^2)
            max_dist <- max(all_distances)
            
            if(max_dist > 6) {
                # Scale down to fit better
                scale_factor <- 6 / max_dist
                layout_df$x[alter_idx] <- layout_df$x[alter_idx] * scale_factor
                layout_df$y[alter_idx] <- layout_df$y[alter_idx] * scale_factor
            }
        }
        
        return(layout_df)
    })
    
    names(ego_centric_layouts) <- names(base_layout)
    return(ego_centric_layouts)
}

#' Remove ego-alter edges from ego network
#' 
#' This function removes all edges between the ego and alters in an ego network,
#' leaving only the alter-alter connections visible. This is useful for visualizing
#' the structure among alters without the clutter of ego connections.
#'
#' @param netlet An ego network created with ego_netify()
#' @return A modified netify object with ego edges removed
#' @export
#' @examples
#' \dontrun{
#' ego_net <- ego_netify(my_network, ego = "Alice")
#' alter_only_net <- remove_ego_edges(ego_net)
#' plot(alter_only_net)
#' }
remove_ego_edges <- function(netlet) {
    # Check if this is an ego network
    if(!isTRUE(attr(netlet, "ego_netify"))) {
        return(netlet)
    }
    
    # Get the ego name
    ego_name <- attr(netlet, "ego_id")
    
    # Process the network to remove ego edges
    if(attr(netlet, "netify_type") == "cross_sec") {
        # Cross-sectional network
        netlet_copy <- netlet
        ego_idx <- which(rownames(netlet_copy) == ego_name)
        if(length(ego_idx) > 0) {
            # Set ego's row and column to 0 (removes all ego edges)
            netlet_copy[ego_idx, ] <- 0
            netlet_copy[, ego_idx] <- 0
        }
        return(netlet_copy)
    } else {
        # Longitudinal network
        netlet_copy <- netlet
        for(t in seq_along(netlet_copy)) {
            ego_idx <- which(rownames(netlet_copy[[t]]) == ego_name)
            if(length(ego_idx) > 0) {
                netlet_copy[[t]][ego_idx, ] <- 0
                netlet_copy[[t]][, ego_idx] <- 0
            }
        }
        return(netlet_copy)
    }
}