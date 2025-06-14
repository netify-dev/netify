#' Adjust edge endpoints to stop at node boundaries
#'
#' This function adjusts the endpoints of edges so that arrows stop at node
#' boundaries rather than pointing to node centers. It handles both straight
#' and curved edges.
#'
#' @param edge_data Data frame with edge coordinates (x1, y1, x2, y2, from, to)
#' @param node_data Data frame with node positions (x, y, name) and sizes
#' @param node_size Either a single numeric value or column name for node sizes
#' @param size_scale Scaling factor to convert ggplot2 size units to coordinate units
#' @param arrow_gap Additional gap between arrow tip and node boundary (0-1)
#' @param curved Logical indicating whether edges are curved
#' @param curvature Curvature amount if edges are curved
#' 
#' @return Updated edge_data with adjusted endpoints
#' 
#' @keywords internal
#' @noRd

adjust_edge_endpoints <- function(edge_data, node_data, node_size = 1.5, 
                                size_scale = 0.015, arrow_gap = 0.2,
                                curved = FALSE, curvature = 0.5) {
  
  # early return if there are no edges to process
  if (nrow(edge_data) == 0) return(edge_data)
  
  # create lookup tables for node positions (x and y coordinates)
  node_x <- setNames(node_data$x, node_data$name)
  node_y <- setNames(node_data$y, node_data$name)
  
  # figure out node sizes - can be fixed or variable
  if (is.character(node_size) && length(node_size) == 1 && node_size %in% names(node_data)) {
    # use variable sizes from a column in the node data
    node_sizes <- node_data[[node_size]]
    # check if the column name suggests a transformation (e.g., "log(degree)")
    if (grepl("^log\\(", node_size)) {
      # assume the data is already transformed, no extra work needed here
    }
  } else if (is.numeric(node_size)) {
    # use a fixed size for all nodes
    node_sizes <- rep(node_size, nrow(node_data))
  } else {
    # fallback to a default size for nodes
    node_sizes <- rep(1.5, nrow(node_data))
  }
  
  # create a lookup table for node sizes based on node names
  node_size_lookup <- setNames(node_sizes, node_data$name)
  
  # convert node sizes to radii in plot coordinates
  # note: ggplot2 point size is roughly diameter in mm, so we calculate radius in plot units
  node_radii <- sqrt(node_size_lookup / pi) * size_scale * (1 + arrow_gap)
  
  if (!curved) {
    # adjust straight edges to account for node positions and radii
    edge_data <- adjust_straight_edges(edge_data, node_x, node_y, node_radii)
  } else {
    # adjust curved edges to account for node positions, radii, and curvature
    edge_data <- adjust_curved_edges(edge_data, node_x, node_y, node_radii, curvature)
  }
  
  # 
  return(edge_data)
}

#' Adjust straight edge endpoints
#'
#' @param edge_data Edge data frame
#' @param node_x Named vector of node x positions
#' @param node_y Named vector of node y positions
#' @param node_radii Named vector of node radii in coordinate units
#' 
#' @return Updated edge_data
#' @keywords internal
#' @noRd

adjust_straight_edges <- function(edge_data, node_x, node_y, node_radii) {
  
  # get positions of the edges
  x1 <- edge_data$x1
  y1 <- edge_data$y1
  x2 <- edge_data$x2
  y2 <- edge_data$y2
  
  # calculate direction vectors (dx and dy)
  dx <- x2 - x1
  dy <- y2 - y1
  d <- sqrt(dx^2 + dy^2) # find the distance between points
  
  # avoid divison by zero (this can cause errors)
  d[d == 0] <- 1
  
  # normalize the direction vectors (make them unit length)
  dx_norm <- dx / d
  dy_norm <- dy / d
  
  # get the radii of the target nodes
  target_radii <- node_radii[edge_data$to]
  target_radii[is.na(target_radii)] <- 0 # replace missing radii with 0
  
  # adjust the target endpoint (move it closer to the target node)
  edge_data$x2 <- x2 - dx_norm * target_radii
  edge_data$y2 <- y2 - dy_norm * target_radii
  
  # optionally adjust the source endpoint (move it away from the source node)
  # source_radii <- node_radii[edge_data$from] # get radii of source nodes
  # source_radii[is.na(source_radii)] <- 0 # handle missing radii
  # edge_data$x1 <- x1 + dx_norm * source_radii # adjust x1
  # edge_data$y1 <- y1 + dy_norm * source_radii # adjust y1
  
  # 
  return(edge_data)
}

#' Adjust curved edge endpoints
#'
#' @param edge_data Edge data frame
#' @param node_x Named vector of node x positions
#' @param node_y Named vector of node y positions
#' @param node_radii Named vector of node radii
#' @param curvature Curvature parameter
#' 
#' @return Updated edge_data
#' @keywords internal
#' @noRd

adjust_curved_edges <- function(edge_data, node_x, node_y, node_radii, curvature) {
  
  # for curved edges, we approximate by calculating the tangent near the endpoint
  # this is a simple approach that works well for moderate curvatures
  # it won't be perfect for extreme cases, but it's good enough for most visualizations
  
  for (i in seq_len(nrow(edge_data))) {
    x1 <- edge_data$x1[i]
    y1 <- edge_data$y1[i]
    x2 <- edge_data$x2[i]
    y2 <- edge_data$y2[i]
    
    # find the midpoint of the edge
    mx <- (x1 + x2) / 2
    my <- (y1 + y2) / 2
    
    # calculate the direction perpendicular to the edge
    dx <- x2 - x1
    dy <- y2 - y1
    d <- sqrt(dx^2 + dy^2)
    
    # skip if the edge length is zero (shouldn't happen, but just in case)
    if (d == 0) next
    
    # compute the perpendicular vector
    px <- -dy / d
    py <- dx / d
    
    # offset the control point to create curvature
    offset <- d * curvature / 2
    cx <- mx + px * offset
    cy <- my + py * offset
    
    # approximate the tangent at the target using the control point
    tx <- x2 - cx
    ty <- y2 - cy
    td <- sqrt(tx^2 + ty^2)
    
    # skip if the tangent length is zero (again, just being cautious)
    if (td == 0) next
    
    # normalize the tangent vector
    tx <- tx / td
    ty <- ty / td
    
    # adjust the endpoint to account for the target node's radius
    target_radius <- node_radii[edge_data$to[i]]
    if (!is.na(target_radius) && target_radius > 0) {
      edge_data$x2[i] <- x2 - tx * target_radius
      edge_data$y2[i] <- y2 - ty * target_radius
    }
  }
  return(edge_data)
}

#' Calculate appropriate size scale factor
#'
#' Estimates the scale factor needed to convert ggplot2 size units to
#' coordinate units based on the plot's coordinate range.
#'
#' @param node_data Data frame with x and y coordinates
#' @param base_scale Base scale factor (default 0.015)
#' 
#' @return Adjusted scale factor
#' @keywords internal
#' @noRd

calculate_size_scale <- function(node_data, base_scale = 0.015) {
  # get coordinate range
  x_range <- diff(range(node_data$x, na.rm = TRUE))
  y_range <- diff(range(node_data$y, na.rm = TRUE))
  
  # use the smaller range to be conservative
  coord_range <- min(x_range, y_range)
  
  # adjust scale based on coordinate range
  # most layout algorithms produce coordinates in [-1, 1] or similar
  if (coord_range < 5) {
    # small coordinate range, use larger scale
    return(base_scale * 2)
  } else if (coord_range > 50) {
    # large coordinate range, use smaller scale
    return(base_scale / 2)
  } else {
    return(base_scale)
  }
}