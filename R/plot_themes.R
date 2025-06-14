#' theme_netify function
#'
#' This function returns a customized theme for netify plots.
#' It is based on the `theme_minimal` function from the `ggplot2` package.
#' It removes axis text and titles from the plot.
#'
#' @return A customized theme object for netify plots.
#' @author Cassy Dorff, Shahryar Minhas
#' @import ggplot2
#' @export theme_netify
#'

theme_netify = function(){
	theme_minimal() + 
	theme(
		axis.text = element_blank(),
		axis.title = element_blank(),
		legend.position='top',
        panel.border=element_blank(),
        axis.ticks=element_blank(),
		strip.text = element_text(color='white'),
		strip.background = element_rect(fill = "#525252", color='#525252')        
	)
}

#' theme_stat_netify function
#'
#' This function returns a customized theme for netify stat plots.
#'
#' @return A customized theme object for netify stat plots.
#' @author Cassy Dorff, Shahryar Minhas
#' @import ggplot2
#' @export theme_stat_netify
#'

theme_stat_netify = function(){
	theme_bw() +
	theme(
		panel.border =element_blank(),
		axis.ticks = element_blank(),
		axis.text.x = element_text(angle = 90, hjust = 1),
		strip.text = element_text(color = "white"), 
		strip.background = element_rect(fill = "#525252", color = "#525252"),
		legend.position = 'bottom'
	)
}

#' Get smart defaults based on network properties
#'
#' Automatically adjusts plotting defaults based on network characteristics
#' such as size, density, and structure.
#'
#' @param netlet A netify object
#' @param msrmnts Measurement data associated with the netlet
#' @param plot_args Existing plot arguments to merge with
#' @return Updated plot_args with smart defaults applied
#' 
#' @author Shahryar Minhas
#' 
#' @keywords internal
#' @noRd

get_smart_defaults <- function(netlet, msrmnts=NULL, plot_args = list()) {

  # if measurements are not provided, try to extract from netlet
  if (is.null(msrmnts)) { msrmnts <- netify_measurements(netlet) }
  
  # Calculate network properties
  if (attr(netlet, 'netify_type') == 'cross_sec') {
    n_nodes <- length(unique(c(msrmnts$row_actors, msrmnts$col_actors)))
    # Get edge count from raw network
    net_raw <- get_raw(netlet)
    n_edges <- sum(net_raw > 0, na.rm = TRUE)
    if (attr(netlet, 'symmetric')) n_edges <- n_edges / 2
  } else {
    # For longitudinal, use average across time
    if (attr(netlet, 'netify_type') == 'longit_list') {
      all_actors <- unique(unlist(c(msrmnts$row_actors, msrmnts$col_actors)))
      n_nodes <- length(all_actors)
      # Average edges across time
      n_edges <- mean(sapply(netlet, function(x) sum(get_raw(x) > 0, na.rm = TRUE)))
    } else {
      n_nodes <- length(unique(c(msrmnts$row_actors, msrmnts$col_actors)))
      net_raw <- get_raw(netlet)
      n_edges <- mean(apply(net_raw, 3, function(x) sum(x > 0, na.rm = TRUE)))
    }
  }
  
  # Calculate density
  max_edges <- if(attr(netlet, 'symmetric')) {
    n_nodes * (n_nodes - 1) / 2
  } else {
    n_nodes * (n_nodes - 1)
  }
  density <- n_edges / max(max_edges, 1)
  
  # Smart defaults based on network properties
  smart_defaults <- list()
  
  # Check if a style has been applied by looking for characteristic style parameters
  has_style <- !is.null(plot_args$point_fill) || 
                !is.null(plot_args$edge_color) ||
                !is.null(plot_args$curve_edges)
  
  # 1. Point size based on number of nodes (unless set by user or stle)
  if (is.null(plot_args$point_size) && is.null(plot_args$node_size)) {
    if (n_nodes > 100) {
      smart_defaults$point_size <- 1.0
    } else if (n_nodes > 50) {
      smart_defaults$point_size <- 1.5
    } else if (n_nodes > 20) {
      smart_defaults$point_size <- 2.0
    } else {
      smart_defaults$point_size <- 2.5
    }
  }
  
  # 2. Edge transparency based on density (don't override if style sets it)
  if (is.null(plot_args$edge_alpha) && is.null(plot_args$edge_alpha_var) && !has_style) {
    if (density > 0.5) {
      smart_defaults$edge_alpha <- 0.1
    } else if (density > 0.3) {
      smart_defaults$edge_alpha <- 0.2
    } else if (density > 0.15) {
      smart_defaults$edge_alpha <- 0.3
    } else {
      smart_defaults$edge_alpha <- 0.5
    }
  }
  
  # 3. Text labels for small networks
  if (is.null(plot_args$add_text) && is.null(plot_args$select_text)) {
    smart_defaults$add_text <- n_nodes <= 15
  }
  
  # 4. Curved edges for small dense networks
  if (is.null(plot_args$curve_edges)) {
    smart_defaults$curve_edges <- n_nodes < 30 && density > 0.3
  }
  
  # 5. Remove isolates by default for large networks
  if (is.null(plot_args$remove_isolates)) {
    smart_defaults$remove_isolates <- n_nodes > 20
  }
  
  # 6. Edge color - slightly muted for better visibility
  if (is.null(plot_args$edge_color)) {
    smart_defaults$edge_color <- "#666666"  # Medium gray
  }
  
  # Merge smart defaults with existing plot_args
  # Existing plot_args take precedence
  for (name in names(smart_defaults)) {
    if (is.null(plot_args[[name]])) {
      plot_args[[name]] <- smart_defaults[[name]]
    }
  }
  
  return(plot_args)
}

#' Apply color palettes based on variable type and mapping
#'
#' Automatically selects appropriate ColorBrewer palettes based on
#' whether variables are continuous or categorical
#'
#' @param plot_args Plot arguments
#' @param net_dfs Network data frames from decompose_netify
#' @return Updated plot_args with color palette specifications
#' 
#' @keywords internal
#' @noRd

apply_smart_palettes <- function(plot_args, net_dfs) {
  
  # Check each variable mapping and determine if continuous or categorical
  
  # Node color mapping
  if (!is.null(plot_args$point_color_var) || !is.null(plot_args$node_color_var)) {
    var_name <- plot_args$point_color_var %||% plot_args$node_color_var
    if (var_name %in% names(net_dfs$nodal_data)) {
      var_data <- net_dfs$nodal_data[[var_name]]
      
      # Determine if categorical or continuous
      if (is.numeric(var_data) && length(unique(var_data)) > 10) {
        # Continuous - use sequential palette
        if (is.null(plot_args$node_color_palette)) {
          plot_args$node_color_palette <- "Blues"
          plot_args$node_color_direction <- 1
        }
      } else {
        # Categorical - use qualitative palette
        n_cats <- length(unique(var_data))
        if (is.null(plot_args$node_color_palette)) {
          if (n_cats <= 8) {
            plot_args$node_color_palette <- "Set2"
          } else if (n_cats <= 12) {
            plot_args$node_color_palette <- "Set3"
          } else {
            plot_args$node_color_palette <- "Paired"
          }
        }
      }
    }
  }
  
  # Node fill mapping
  if (!is.null(plot_args$point_fill_var) || !is.null(plot_args$node_fill_var)) {
    var_name <- plot_args$point_fill_var %||% plot_args$node_fill_var
    if (var_name %in% names(net_dfs$nodal_data)) {
      var_data <- net_dfs$nodal_data[[var_name]]
      
      if (is.numeric(var_data) && length(unique(var_data)) > 10) {
        # Continuous
        if (is.null(plot_args$node_fill_palette)) {
          plot_args$node_fill_palette <- "Oranges"
          plot_args$node_fill_direction <- 1
        }
      } else {
        # Categorical
        n_cats <- length(unique(var_data))
        if (is.null(plot_args$node_fill_palette)) {
          if (n_cats <= 8) {
            plot_args$node_fill_palette <- "Set1"
          } else {
            plot_args$node_fill_palette <- "Set3"
          }
        }
      }
    }
  }
  
  # Edge color mapping
  if (!is.null(plot_args$edge_color_var)) {
    var_name <- plot_args$edge_color_var
    if (var_name %in% names(net_dfs$edge_data)) {
      var_data <- net_dfs$edge_data[[var_name]]
      
      if (is.numeric(var_data) && length(unique(var_data)) > 10) {
        # Continuous - use diverging palette for edges
        if (is.null(plot_args$edge_color_palette)) {
          plot_args$edge_color_palette <- "RdBu"
          plot_args$edge_color_direction <- -1  # Reverse for better visibility
        }
      } else {
        # Categorical edges
        if (is.null(plot_args$edge_color_palette)) {
          plot_args$edge_color_palette <- "Dark2"
        }
      }
    }
  }
  
  return(plot_args)
}

