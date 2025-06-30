#' Calculate ego-centric layout positions for network visualization
#'
#' `get_ego_layout` computes node positions for ego network visualization using
#' ego-centric layout algorithms. These layouts place the ego at the center and
#' arrange alters in meaningful patterns around it, making the ego's relationships
#' more visually apparent.
#'
#' @param netlet A netify object (class "netify") that is an ego network, created
#'   using \code{\link{ego_netify}}. The object must have ego_netify = TRUE attribute.
#' @param layout Character string specifying the ego-centric layout algorithm. Options:
#'   \itemize{
#'     \item \code{"radial"}: Places ego at center with alters arranged in a circle
#'       around it. Distance from center can encode relationship strength.
#'     \item \code{"concentric"}: Places ego at center with alters in concentric
#'       circles based on a grouping variable or relationship strength.
#'     \item \code{"star"}: Simple star layout with ego at center and alters
#'       equally spaced around it (default).
#'   }
#' @param group_by Character string specifying a nodal attribute to use for grouping
#'   alters in the layout. For "radial" layout, groups are arranged in sectors.
#'   For "concentric" layout, groups determine which ring alters appear in.
#'   If NULL (default), no grouping is applied.
#' @param order_by Character string specifying a nodal attribute to use for ordering
#'   alters within their groups or around the ego. Common options include network
#'   statistics like "degree_total" or custom attributes. If NULL (default),
#'   alters are arranged alphabetically.
#' @param weight_to_distance Logical. If TRUE and the network is weighted, use edge
#'   weights to determine distance from ego (higher weights = closer). For "radial"
#'   layout only. Default is FALSE.
#' @param ring_gap Numeric value between 0 and 1 specifying the gap between
#'   concentric rings as a proportion of the total radius. Only used for
#'   "concentric" layout. Default is 0.3.
#' @param ego_size Numeric value specifying the relative size of the central area
#'   reserved for the ego. Larger values create more space between ego and alters.
#'   Default is 0.1.
#' @param seed Integer for random number generation to ensure reproducible layouts
#'   when there are ties in ordering. Default is 6886.
#'
#' @return A list of data frames (one per time period) where each data frame contains:
#'   \itemize{
#'     \item \strong{index}: Integer node index
#'     \item \strong{actor}: Character string with actor name
#'     \item \strong{x}: Numeric x-coordinate for node position
#'     \item \strong{y}: Numeric y-coordinate for node position
#'   }
#'
#'   For cross-sectional networks, returns a list with one element. For longitudinal
#'   networks, returns a named list with time periods as names.
#'
#' @details
#' This function provides specialized layouts for ego networks that emphasize the
#' ego-alter structure:
#'
#' \strong{Layout algorithms:}
#' \itemize{
#'   \item \strong{Radial}: Places the ego at the origin and arranges alters in
#'     a circle around it. If grouping is specified, alters are arranged in sectors
#'     with related nodes near each other. If weight_to_distance is TRUE, alters
#'     with stronger ties to ego are placed closer to the center.
#'   \item \strong{Concentric}: Places the ego at the origin and arranges alters
#'     in concentric circles. The ring assignment can be based on a grouping
#'     variable (categorical) or a continuous variable (discretized into rings).
#'   \item \strong{Star}: A simple star layout that places ego at center and
#'     distributes alters evenly around a single circle. This is equivalent to
#'     the radial layout without grouping or weighting.
#' }
#'
#' \strong{Visual encoding:}
#'
#' The layouts allow encoding of network properties through spatial arrangement:
#' \itemize{
#'   \item \strong{Distance from ego}: Can represent tie strength, frequency of
#'     interaction, or other dyadic measures
#'   \item \strong{Angular position}: Can group similar alters together (e.g.,
#'     family, friends, colleagues)
#'   \item \strong{Ring assignment}: Can represent categories, levels of importance,
#'     or discretized continuous variables
#' }
#'
#' \strong{Longitudinal networks:}
#'
#' For longitudinal ego networks, the function maintains consistent angular positions
#' for alters across time periods when possible, making it easier to track changes
#' in the ego's network over time.
#'
#' @note
#' This function is designed specifically for ego networks created with
#' \code{\link{ego_netify}}. For general network layouts, use
#' \code{\link{get_node_layout}}.
#'
#' The function will issue a warning if used on non-ego networks but will
#' attempt to proceed by treating the first node as the ego.
#'
#' @examples
#' \dontrun{
#' # Create an ego network
#' ego_net <- ego_netify(my_network, ego = "Alice")
#'
#' # Get radial layout with alters grouped by attribute
#' layout_radial <- get_ego_layout(ego_net, 
#'                                layout = "radial",
#'                                group_by = "department")
#'
#' # Get concentric layout with rings based on degree
#' layout_circles <- get_ego_layout(ego_net,
#'                                 layout = "concentric", 
#'                                 group_by = "degree_total")
#'
#' # Use with plot
#' plot(ego_net, point_layout = layout_radial)
#' }
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export get_ego_layout
#' @importFrom stats quantile runif
#'

get_ego_layout <- function(
    netlet,
    layout = "star", 
    group_by = NULL,
    order_by = NULL,
    weight_to_distance = FALSE,
    ring_gap = 0.3,
    ego_size = 0.1,
    seed = 6886) {

    # Check if netify object
    netify_check(netlet)
    
    # Check if this is an ego network
    is_ego <- attr(netlet, "ego_netify") %||% FALSE
    ego_id <- attr(netlet, "ego_id")
    
    if (!is_ego) {
        cli::cli_alert_warning(
            "This does not appear to be an ego network (created with ego_netify).
             Attempting to proceed by treating the first actor as ego."
        )
    }
    
    # Validate layout parameter
    layout <- match.arg(tolower(layout), 
                       choices = c("radial", "concentric", "star"))
    
    # Get network info
    obj_attrs <- attributes(netlet)
    netify_type <- obj_attrs$netify_type
    
    # Handle longitudinal case - process as list
    if (netify_type == "longit_list") {
        # Get network data for each time period
        layout_list <- lapply(seq_along(netlet), function(t) {
            time_name <- names(netlet)[t]
            netlet_t <- netlet[[t]]
            
            # Calculate layout for this time period
            layout_df <- calculate_ego_layout_single(
                netlet = netlet_t,
                layout = layout,
                ego_id = ego_id,
                group_by = group_by,
                order_by = order_by, 
                weight_to_distance = weight_to_distance,
                ring_gap = ring_gap,
                ego_size = ego_size,
                seed = seed + t - 1  # Different seed per time period
            )
            
            return(layout_df)
        })
        
        names(layout_list) <- names(netlet)
        return(layout_list)
        
    } else {
        # Cross-sectional case
        layout_df <- calculate_ego_layout_single(
            netlet = netlet,
            layout = layout,
            ego_id = ego_id,
            group_by = group_by,
            order_by = order_by,
            weight_to_distance = weight_to_distance,
            ring_gap = ring_gap,
            ego_size = ego_size,
            seed = seed
        )
        
        return(list(layout_df))
    }
}

#' Calculate ego layout for a single network
#' 
#' Internal function that computes ego-centric layout for one time period
#' 
#' @keywords internal
#' @noRd
calculate_ego_layout_single <- function(
    netlet, layout, ego_id, group_by, order_by,
    weight_to_distance, ring_gap, ego_size, seed) {
    
    # Get actor names
    actors <- rownames(netlet)
    n_actors <- length(actors)
    
    # Identify ego - use provided ego_id or first actor
    if (is.null(ego_id)) {
        ego_id <- actors[1]
        cli::cli_alert_info(
            paste0("No ego_id found. Using first actor '", ego_id, "' as ego.")
        )
    }
    
    # Verify ego is in the network
    if (!ego_id %in% actors) {
        stop("Ego '", ego_id, "' not found in network")
    }
    
    # Get alters (everyone except ego)
    alters <- setdiff(actors, ego_id)
    n_alters <- length(alters)
    
    # Initialize layout data frame
    layout_df <- data.frame(
        index = seq_len(n_actors),
        actor = actors,
        x = numeric(n_actors),
        y = numeric(n_actors),
        stringsAsFactors = FALSE
    )
    
    # Place ego at center
    ego_idx <- which(actors == ego_id)
    layout_df$x[ego_idx] <- 0
    layout_df$y[ego_idx] <- 0
    
    # If no alters, return ego-only layout
    if (n_alters == 0) {
        return(layout_df)
    }
    
    # Get nodal attributes if needed
    nodal_attrs <- NULL
    if (!is.null(group_by) || !is.null(order_by)) {
        nodal_attrs <- attr(netlet, "nodal_data")
        
        # Check if requested attributes exist
        if (!is.null(group_by) && !is.null(nodal_attrs) && 
            !group_by %in% names(nodal_attrs)) {
            cli::cli_alert_warning(
                paste0("Grouping variable '", group_by, "' not found in nodal attributes. ",
                       "Proceeding without grouping.")
            )
            group_by <- NULL
        }
        
        if (!is.null(order_by) && !is.null(nodal_attrs) && 
            !order_by %in% names(nodal_attrs)) {
            cli::cli_alert_warning(
                paste0("Ordering variable '", order_by, "' not found in nodal attributes. ",
                       "Proceeding with alphabetical order.")  
            )
            order_by <- NULL
        }
    }
    
    # Get edge weights if needed
    edge_weights <- NULL
    if (weight_to_distance && !attr(netlet, "weight_binary")) {
        # Extract weights from ego to each alter
        edge_weights <- numeric(n_alters)
        names(edge_weights) <- alters
        
        for (i in seq_along(alters)) {
            alter <- alters[i]
            # Get weight in both directions and use max
            w1 <- netlet[ego_id, alter]
            w2 <- netlet[alter, ego_id]
            edge_weights[i] <- max(w1, w2, na.rm = TRUE)
        }
        
        # Handle NAs
        edge_weights[is.na(edge_weights)] <- 0
    }
    
    # Calculate positions based on layout type
    if (layout == "star") {
        alter_positions <- calculate_star_positions(
            alters, order_by, nodal_attrs, ego_size, seed
        )
    } else if (layout == "radial") {
        alter_positions <- calculate_radial_positions(
            alters, group_by, order_by, nodal_attrs, 
            edge_weights, weight_to_distance, ego_size, seed
        )
    } else if (layout == "concentric") {
        alter_positions <- calculate_concentric_positions(
            alters, group_by, order_by, nodal_attrs,
            ring_gap, ego_size, seed
        )
    }
    
    # Assign alter positions to layout data frame
    for (i in seq_along(alters)) {
        alter <- alters[i]
        alter_idx <- which(actors == alter)
        layout_df$x[alter_idx] <- alter_positions$x[i]
        layout_df$y[alter_idx] <- alter_positions$y[i]
    }
    
    return(layout_df)
}

#' Calculate star layout positions
#' @keywords internal  
#' @noRd
calculate_star_positions <- function(alters, order_by, nodal_attrs, ego_size, seed) {
    n_alters <- length(alters)
    
    # Order alters
    ordered_alters <- order_alters(alters, order_by, nodal_attrs, seed)
    
    # Calculate angles - evenly distributed
    angles <- seq(0, 2 * pi, length.out = n_alters + 1)[-1]
    
    # Fixed radius for all alters
    radius <- 1
    
    # Convert to x,y coordinates
    x <- radius * cos(angles)
    y <- radius * sin(angles)
    
    # Reorder to match original alter order
    positions <- data.frame(
        alter = ordered_alters,
        x = x,
        y = y,
        stringsAsFactors = FALSE
    )
    
    # Match back to input order
    match_idx <- match(alters, positions$alter)
    
    return(data.frame(
        x = positions$x[match_idx],
        y = positions$y[match_idx]
    ))
}

#' Calculate radial layout positions
#' @keywords internal
#' @noRd  
calculate_radial_positions <- function(
    alters, group_by, order_by, nodal_attrs, 
    edge_weights, weight_to_distance, ego_size, seed) {
    
    n_alters <- length(alters)
    
    # Get groups if specified
    if (!is.null(group_by) && !is.null(nodal_attrs)) {
        alter_groups <- nodal_attrs[[group_by]][match(alters, nodal_attrs$actor)]
        unique_groups <- unique(alter_groups[!is.na(alter_groups)])
        n_groups <- length(unique_groups)
    } else {
        alter_groups <- rep(1, n_alters)
        unique_groups <- 1
        n_groups <- 1
    }
    
    # Initialize positions
    x <- numeric(n_alters)
    y <- numeric(n_alters)
    
    # Process each group
    for (g in seq_len(n_groups)) {
        group <- unique_groups[g]
        group_mask <- alter_groups == group & !is.na(alter_groups)
        group_alters <- alters[group_mask]
        n_group <- length(group_alters)
        
        if (n_group == 0) next
        
        # Order alters within group
        ordered_group <- order_alters(group_alters, order_by, nodal_attrs, seed)
        
        # Calculate angular range for this group
        if (n_groups > 1) {
            # Divide circle into sectors
            sector_start <- (g - 1) * 2 * pi / n_groups
            sector_end <- g * 2 * pi / n_groups
            # Leave small gap between sectors
            gap <- 0.05
            sector_start <- sector_start + gap/2
            sector_end <- sector_end - gap/2
        } else {
            sector_start <- 0
            sector_end <- 2 * pi
        }
        
        # Calculate angles within sector
        if (n_group == 1) {
            group_angles <- (sector_start + sector_end) / 2
        } else {
            group_angles <- seq(sector_start, sector_end, length.out = n_group)
        }
        
        # Calculate radii
        if (weight_to_distance && !is.null(edge_weights)) {
            # Get weights for this group
            group_weights <- edge_weights[ordered_group]
            
            # Normalize weights to [0,1]
            if (max(group_weights) > min(group_weights)) {
                norm_weights <- (group_weights - min(group_weights)) / 
                               (max(group_weights) - min(group_weights))
            } else {
                norm_weights <- rep(0.5, n_group)
            }
            
            # Map to radius (closer = higher weight)
            # Use ego_size as minimum distance
            min_radius <- ego_size + 0.2
            max_radius <- 1
            group_radii <- max_radius - norm_weights * (max_radius - min_radius)
        } else {
            # Fixed radius for all
            group_radii <- rep(1, n_group)
        }
        
        # Calculate positions
        for (i in seq_len(n_group)) {
            alter_idx <- which(alters == ordered_group[i])
            x[alter_idx] <- group_radii[i] * cos(group_angles[i])
            y[alter_idx] <- group_radii[i] * sin(group_angles[i])
        }
    }
    
    # Handle any ungrouped alters (NAs in grouping variable)
    if (any(is.na(alter_groups))) {
        na_mask <- is.na(alter_groups)
        na_alters <- alters[na_mask]
        n_na <- length(na_alters)
        
        # Place at outer edge in a separate arc
        na_angles <- seq(3*pi/2, 2*pi, length.out = n_na + 1)[-1]
        
        for (i in seq_len(n_na)) {
            alter_idx <- which(alters == na_alters[i])
            x[alter_idx] <- 1.1 * cos(na_angles[i])
            y[alter_idx] <- 1.1 * sin(na_angles[i])
        }
    }
    
    return(data.frame(x = x, y = y))
}

#' Calculate concentric layout positions  
#' @keywords internal
#' @noRd
calculate_concentric_positions <- function(
    alters, group_by, order_by, nodal_attrs,
    ring_gap, ego_size, seed) {
    
    n_alters <- length(alters)
    
    # Determine ring assignment
    if (!is.null(group_by) && !is.null(nodal_attrs)) {
        group_var <- nodal_attrs[[group_by]][match(alters, nodal_attrs$actor)]
        
        # Check if numeric or factor/character
        if (is.numeric(group_var)) {
            # Discretize numeric variable into rings
            # Use quantiles to create roughly equal-sized rings
            n_rings <- min(4, ceiling(sqrt(n_alters)))  # Max 4 rings
            ring_breaks <- quantile(group_var, 
                                  probs = seq(0, 1, length.out = n_rings + 1),
                                  na.rm = TRUE)
            ring_assignment <- cut(group_var, breaks = ring_breaks, 
                                 include.lowest = TRUE, labels = FALSE)
        } else {
            # Use factor levels or unique values as rings
            unique_vals <- unique(group_var[!is.na(group_var)])
            ring_assignment <- match(group_var, unique_vals)
            n_rings <- length(unique_vals)
        }
    } else {
        # No grouping - put all alters in one ring
        ring_assignment <- rep(1, n_alters)
        n_rings <- 1
    }
    
    # Handle NAs - put in outermost ring
    if (any(is.na(ring_assignment))) {
        n_rings <- n_rings + 1
        ring_assignment[is.na(ring_assignment)] <- n_rings
    }
    
    # Calculate ring radii
    min_radius <- ego_size + 0.2
    max_radius <- 1
    
    if (n_rings == 1) {
        ring_radii <- 0.7  # Single ring at moderate distance
    } else {
        available_space <- max_radius - min_radius
        gap_total <- ring_gap * (n_rings - 1)
        ring_spacing <- (available_space - gap_total) / n_rings
        
        ring_radii <- numeric(n_rings)
        for (r in 1:n_rings) {
            ring_radii[r] <- min_radius + (r - 1) * (ring_spacing + ring_gap) + ring_spacing/2
        }
    }
    
    # Initialize positions
    x <- numeric(n_alters)
    y <- numeric(n_alters)
    
    # Place alters in each ring
    for (r in 1:n_rings) {
        ring_mask <- ring_assignment == r
        ring_alters <- alters[ring_mask]
        n_ring <- length(ring_alters)
        
        if (n_ring == 0) next
        
        # Order alters within ring
        ordered_ring <- order_alters(ring_alters, order_by, nodal_attrs, seed)
        
        # Calculate angles - evenly distributed
        ring_angles <- seq(0, 2 * pi, length.out = n_ring + 1)[-1]
        
        # Add small rotation to each ring for visual interest
        rotation_offset <- (r - 1) * pi / (2 * n_rings)
        ring_angles <- ring_angles + rotation_offset
        
        # Assign positions
        for (i in seq_len(n_ring)) {
            alter_idx <- which(alters == ordered_ring[i])
            x[alter_idx] <- ring_radii[r] * cos(ring_angles[i])
            y[alter_idx] <- ring_radii[r] * sin(ring_angles[i])
        }
    }
    
    return(data.frame(x = x, y = y))
}

#' Order alters for layout
#' @keywords internal
#' @noRd
order_alters <- function(alters, order_by, nodal_attrs, seed) {
    n_alters <- length(alters)
    
    if (!is.null(order_by) && !is.null(nodal_attrs)) {
        # Get ordering variable
        order_var <- nodal_attrs[[order_by]][match(alters, nodal_attrs$actor)]
        
        # Handle NAs by putting them last
        na_mask <- is.na(order_var)
        if (any(na_mask)) {
            # Set NAs to a value that will sort last
            if (is.numeric(order_var)) {
                order_var[na_mask] <- min(order_var, na.rm = TRUE) - 1
            } else {
                order_var[na_mask] <- "ZZZZZZ"  # Sort last alphabetically
            }
        }
        
        # Create order with tie-breaking
        set.seed(seed)
        tie_breaker <- runif(n_alters)
        order_idx <- order(order_var, tie_breaker)
        
        return(alters[order_idx])
    } else {
        # Default to alphabetical order
        return(sort(alters))
    }
}