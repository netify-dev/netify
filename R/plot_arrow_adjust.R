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
                                  size_scale = NULL, arrow_gap = 0.05,
                                  curved = FALSE, curvature = 0.5) {
    # return early if there are no edges to process
    if (nrow(edge_data) == 0) {
        return(edge_data)
    }

    # create lookup tables for node positions (x and y coordinates)
    node_x <- setNames(node_data$x, node_data$name)
    node_y <- setNames(node_data$y, node_data$name)

    # determine node sizes
    # if node_size is a column name in node_data, use variable sizes from that column
    if (is.character(node_size) && length(node_size) == 1 && node_size %in% names(node_data)) {
        node_sizes <- node_data[[node_size]]
    } else if (is.numeric(node_size)) {
        # if node_size is numeric, use a fixed size for all nodes
        node_sizes <- rep(node_size, nrow(node_data))
    } else {
        # fallback to a default size for nodes
        node_sizes <- rep(1.5, nrow(node_data))
    }

    # create a lookup table for node sizes based on node names
    node_size_lookup <- setNames(node_sizes, node_data$name)

    # calculate size scale if not provided
    if (is.null(size_scale)) {
        # get the range of x and y coordinates to determine the plot range
        x_range <- diff(range(node_data$x, na.rm = TRUE))
        y_range <- diff(range(node_data$y, na.rm = TRUE))
        plot_range <- min(x_range, y_range)

        # derive a scale factor for node sizes based on the plot range
        # this formula ensures sizes are proportional to the plot's coordinate system
        size_scale <- 0.01 * sqrt(plot_range)
    }

    # convert node sizes to radii in plot coordinates
    # ggplot2 sizes roughly represent the diameter, so divide by 2 for radius
    node_radii <- (node_sizes / 2) * size_scale

    # add a small gap to the radii to account for arrow spacing
    node_radii <- node_radii + (arrow_gap * size_scale)

    # adjust edges based on whether they are straight or curved
    if (!curved) {
        # adjust straight edges to account for node positions and radii
        edge_data <- adjust_straight_edges(edge_data, node_x, node_y, node_radii)
    } else {
        # adjust curved edges to account for node positions, radii, and curvature
        edge_data <- adjust_curved_edges(edge_data, node_x, node_y, node_radii, curvature)
    }

    # return the adjusted edge data
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

    # calculate the direction vectors (dx and dy)
    dx <- x2 - x1
    dy <- y2 - y1
    d <- sqrt(dx^2 + dy^2) # calculate the distance between the two points

    # avoid division by zero by setting zero-length edges to a distance of 1
    zero_length <- d == 0
    d[zero_length] <- 1

    # normalize the direction vectors to unit length
    dx_norm <- dx / d
    dy_norm <- dy / d

    # get the radii of the target nodes (nodes the edges point to)
    target_radii <- node_radii[edge_data$to]
    target_radii[is.na(target_radii)] <- 0 # replace missing radii with 0

    # create a mask for edges that need adjustment
    adjustment_mask <- !zero_length & target_radii > 0

    # adjust the target endpoint to move it closer to the source node
    edge_data$x2[adjustment_mask] <- x2[adjustment_mask] - dx_norm[adjustment_mask] * target_radii[adjustment_mask]
    edge_data$y2[adjustment_mask] <- y2[adjustment_mask] - dy_norm[adjustment_mask] * target_radii[adjustment_mask]

    # optionally, you can adjust the source endpoint to move it away from the source node
    # uncomment the following lines if you want to adjust the source endpoint
    # source_radii <- node_radii[edge_data$from] # get radii of source nodes
    # source_radii[is.na(source_radii)] <- 0 # handle missing radii
    # adjustment_mask_source <- !zero_length & source_radii > 0
    # edge_data$x1[adjustment_mask_source] <- x1[adjustment_mask_source] + dx_norm[adjustment_mask_source] * source_radii[adjustment_mask_source]
    # edge_data$y1[adjustment_mask_source] <- y1[adjustment_mask_source] + dy_norm[adjustment_mask_source] * source_radii[adjustment_mask_source]

    # return the adjusted edge data
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
    # loop through each edge in the data
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

        # skip processing if the edge length is zero
        if (d == 0) next

        # compute the perpendicular vector (used for curvature)
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

        # skip processing if the tangent length is zero
        if (td == 0) next

        # normalize the tangent vector (to ensure consistent direction)
        tx <- tx / td
        ty <- ty / td

        # adjust the endpoint to account for the target node's radius
        target_radius <- node_radii[edge_data$to[i]]
        if (!is.na(target_radius) && target_radius > 0) {
            edge_data$x2[i] <- x2 - tx * target_radius
            edge_data$y2[i] <- y2 - ty * target_radius
        }
    }

    # return the modified edge data
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

calculate_size_scale <- function(node_data, base_scale = 0.01) {
    # calculate the range of x coordinates
    x_range <- diff(range(node_data$x, na.rm = TRUE))

    # calculate the range of y coordinates
    y_range <- diff(range(node_data$y, na.rm = TRUE))

    # combine the x and y ranges using the geometric mean
    # this helps make the scaling more stable and less sensitive to extreme values
    coord_range <- sqrt(x_range * y_range)

    # scale the base factor by the square root of the combined coordinate range
    # this ensures the scaling adjusts gradually based on the size of the data
    return(base_scale * sqrt(coord_range))
}
