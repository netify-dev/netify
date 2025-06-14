#' Apply style to netify plot
#'
#' Helper function to properly apply styles to netify plots
#'
#' @param netlet A netify object
#' @param style_fun A style function (e.g., style_budapest)
#' @param ... Additional plot parameters
#' @return A ggplot object
#' @export

plot_with_style <- function(netlet, style_fun, ...) {
  # Get the style parameters
  style_params <- style_fun()
  
  # Get additional parameters
  additional_params <- list(...)
  
  # Combine them (additional params override style params)
  all_params <- c(style_params, additional_params)
  
  # Call plot with combined parameters
  do.call(plot, c(list(x = netlet), all_params))
}

#' Preset style for bipartite networks
#'
#' A complete visual style optimized for displaying bipartite networks
#' with two distinct node sets
#'
#' @return List of plot arguments
#' @export

style_bipartite_network <- function() {
  list(
    # Edges
    edge_color = "#7F8C8D",
    edge_alpha = 0.4,
    
    # Will need to be combined with manual shape/color by node type
    point_size = 3,
    point_stroke = 1,
    
    # No curves for clarity
    curve_edges = FALSE,
    
    # Often want labels for bipartite
    add_text = TRUE,
    text_size = 2.5,
    check_overlap = TRUE,
    
    # Use bipartite layout
    layout = "bipartite"
  )
}

#' Preset style for temporal/longitudinal networks
#'
#' A complete visual style optimized for displaying networks over time
#'
#' @return List of plot arguments
#' @export

style_temporal_network <- function() {
  list(
    # Subtle edges
    edge_color = "#95A5A6",
    edge_alpha = 0.3,
    edge_linewidth = 0.5,
    
    # Nodes that pop
    point_fill = "#3498DB",
    point_color = "#2980B9",
    point_size = 2,
    
    # Static positions for comparison
    static_actor_positions = TRUE,
    
    # No curves for clarity across time
    curve_edges = FALSE,
    
    # Minimal style works well with facets
    use_style_netify = TRUE
  )
}

#' Wes Anderson-inspired network style (Grand Budapest Hotel)
#'
#' A network style inspired by the pastel color palette of 
#' Wes Anderson's Grand Budapest Hotel
#'
#' @return List of plot arguments
#' @export

style_budapest <- function() {
  list(
    # Soft pink edges
    edge_color = "#F4B5BD",
    edge_alpha = 0.6,
    edge_linewidth = 0.8,
    
    # Nodes in purple and gold
    point_fill = "#9A8C98",
    point_color = "#C9ADA7", 
    point_stroke = 1.5,
	point_shape = 21,
    
    # Curved edges for whimsical feel
    curve_edges = TRUE,
    edge_curvature = 0.3,
    
    # Labels in dark purple
    text_color = "#4A4E69",
    text_size = 3,
    
    # Light background
    use_style_netify = FALSE
  )
}

#' Economist-style network style
#'
#' A network style inspired by The Economist's distinctive
#' visual style with red accents and clean lines
#'
#' @return List of plot arguments
#' @export

style_economist <- function() {
  list(
    # Signature Economist red for edges
    edge_color = "#E3120B",
    edge_alpha = 0.7,
    edge_linewidth = 0.5,
    
    # Light blue nodes
    point_fill = "#6BABEB",
    point_color = "#01621E",  # Dark green border
    point_stroke = 1,
    point_size = 2.5,
	point_shape = 21,	
    
    # No curves - clean geometric look
    curve_edges = FALSE,
    
    # Clean, minimal text
    text_family = "sans",
    text_size = 3,
    text_color = "#000000",
    
    # White background
    use_style_netify = TRUE
  )
}

#' FiveThirtyEight-inspired network style
#'
#' A network style inspired by FiveThirtyEight's data visualization
#' style with gray background and bright accents
#'
#' @return List of plot arguments
#' @export

style_538 <- function() {
  list(
    # Orange edges
    edge_color = "#FC7634",
    edge_alpha = 0.6,
    edge_linewidth = 1,
    
    # Teal nodes
    point_fill = "#30A9D9",
    point_color = "#2A2A2A",
    point_stroke = 0.5,
    point_size = 3,
	point_shape = 21,	
    
    # Straight edges
    curve_edges = FALSE,
    
    # Bold text
    text_size = 3.5,
    text_color = "#2A2A2A",
    text_fontface = 2,  # Bold
    
    # Gray background (apply manually)
    use_style_netify = FALSE
  )
}

#' Solarized-inspired network style
#'
#' A network style based on the popular Solarized color scheme,
#' optimized for reduced eye strain
#'
#' @return List of plot arguments
#' @export

style_solarized <- function() {
  list(
    # Base yellow edges
    edge_color = "#B58900",
    edge_alpha = 0.5,
    edge_linewidth = 0.8,
    
    # Blue nodes with orange border
    point_fill = "#268BD2",
    point_color = "#CB4B16",
    point_stroke = 1,
    point_size = 2.5,
	point_shape = 21,	
    
    # Slight curves
    curve_edges = TRUE,
    edge_curvature = 0.15,
    
    # Readable text
    text_color = "#586E75",
    text_size = 3,
    
    # Dark background (apply manually)
    use_style_netify = FALSE
  )
}

#' Stata-inspired network style
#'
#' A network style that mimics Stata's default graph colors
#' with navy blue and maroon
#'
#' @return List of plot arguments
#' @export

style_stata <- function() {
  list(
    # Navy blue edges
    edge_color = "#1A476F",
    edge_alpha = 0.8,
    edge_linewidth = 0.6,
    
    # Maroon nodes
    point_fill = "#90353B",
    point_color = "#1A476F",
    point_stroke = 1,
    point_size = 2,
	point_shape = 21,	
    
    # No curves - traditional feel
    curve_edges = FALSE,
    
    # Classic serif text
    text_family = "serif",
    text_size = 3,
    text_color = "#000000",
    
    use_style_netify = TRUE
  )
}

#' Nature journal-inspired network style
#'
#' A network style inspired by Nature journal's clean scientific
#' visualization style
#'
#' @return List of plot arguments
#' @export

style_nature <- function() {
  list(
    # Dark blue edges
    edge_color = "#003F7F",
    edge_alpha = 0.6,
    edge_linewidth = 0.7,
    
    # Orange-red nodes
    point_fill = "#ED6D35",
    point_color = "#003F7F",
    point_stroke = 0.8,
    point_size = 2.2,
	point_shape = 21,	
    
    # Clean straight lines
    curve_edges = FALSE,
    
    # Professional text
    text_size = 2.8,
    text_color = "#000000",
    
    use_style_netify = TRUE
  )
}

#' Cyberpunk-inspired network style
#'
#' A futuristic network style with neon colors on dark background
#'
#' @return List of plot arguments
#' @export

style_cyberpunk <- function() {
  list(
    # Neon pink edges
    edge_color = "#FF006E",
    edge_alpha = 0.8,
    edge_linewidth = 1,
    
    # Cyan nodes
    point_fill = "#00F5FF",
    point_color = "#FF006E",
    point_stroke = 1.5,
    point_size = 2.5,
	point_shape = 21,	
    
    # Angular feel
    curve_edges = FALSE,
    
    # Glowing text effect (white text)
    text_color = "#FFFFFF",
    text_size = 3,
    
    # Dark background needed
    use_style_netify = FALSE
  )
}

#' Minimal Tufte-inspired network style
#'
#' A minimalist network style inspired by Edward Tufte's
#' data visualization principles
#'
#' @return List of plot arguments
#' @export

style_tufte <- function() {
  list(
    # Very light gray edges
    edge_color = "#CCCCCC",
    edge_alpha = 0.5,
    edge_linewidth = 0.3,
    
    # Small black nodes
    point_fill = "#000000",
    point_color = "#000000",
    point_stroke = 0,
    point_size = 1.5,
	point_shape = 21,	
    
    # No curves - clean
    curve_edges = FALSE,
    
    # Minimal text
    text_size = 2.5,
    text_color = "#333333",
    
    # Remove all chrome
    use_style_netify = TRUE
  )
}

#' Pastel rainbow network style
#'
#' A soft, cheerful network style using pastel colors
#'
#' @return List of plot arguments
#' @export

style_pastel <- function() {
  list(
    # Lavender edges
    edge_color = "#BB9CC0",
    edge_alpha = 0.7,
    edge_linewidth = 1.2,
    
    # Mint green nodes
    point_fill = "#9EDAC0",
    point_color = "#FEB1D3",  # Pink border
    point_stroke = 1.5,
    point_size = 3,
	point_shape = 21,	
    
    # Soft curves
    curve_edges = TRUE,
    edge_curvature = 0.4,
    
    # Soft gray text
    text_color = "#666666",
    text_size = 3,
    
    use_style_netify = TRUE
  )
}

#' Retro 80s-inspired network style
#'
#' A network style channeling 1980s design aesthetics
#' with bold colors and geometric shapes
#'
#' @return List of plot arguments
#' @export

style_retro80s <- function() {
  list(
    # Hot pink edges
    edge_color = "#FF1493",
    edge_alpha = 0.9,
    edge_linewidth = 1.5,
    
    # Electric blue nodes
    point_fill = "#00CED1",
    point_color = "#FFD700",  # Gold border
    point_stroke = 2,
    point_size = 3.5,
    point_shape = 24,  # Triangle
    
    # No curves - geometric
    curve_edges = FALSE,
    
    # Bold text
    text_size = 4,
    text_color = "#FF1493",
    text_fontface = 2,
    
    use_style_netify = TRUE
  )
}

#' ColorBrewer Dark2-based network style
#'
#' A network style using ColorBrewer's Dark2 palette
#' for high contrast and accessibility
#'
#' @return List of plot arguments
#' @export

style_dark2 <- function() {
  list(
    # Dark orange edges
    edge_color = "#D95F02",
    edge_alpha = 0.6,
    edge_linewidth = 0.8,
    
    # Dark green nodes with purple border
    point_fill = "#1B9E77",
    point_color = "#7570B3",
    point_stroke = 1.2,
    point_size = 2.5,
	point_shape = 21,	
    
    # Slight curves
    curve_edges = TRUE, 
    edge_curvature = 0.2,
    
    # Dark text
    text_color = "#666666",
    text_size = 3,
    
    use_style_netify = TRUE
  )
}

#' List available network styles
#'
#' Shows all available preset network styles with descriptions
#'
#' @return A data frame with style names and descriptions
#' @export

list_network_styles <- function() {
  styles <- data.frame(
    style = c(
      "style_budapest",
      "style_economist", 
      "style_538",
      "style_solarized",
      "style_stata",
      "style_nature",
      "style_cyberpunk",
      "style_tufte",
      "style_pastel",
      "style_retro80s",
      "style_dark2"
    ),
    description = c(
      "Wes Anderson's Grand Budapest Hotel pastels",
      "The Economist's signature red and blue",
      "FiveThirtyEight's orange and teal on gray",
      "Solarized color scheme for reduced eye strain",
      "Stata's classic navy and maroon",
      "Nature journal's scientific blue and orange",
      "Neon cyberpunk with pink and cyan",
      "Edward Tufte's minimalist approach",
      "Soft pastel colors for a gentle look",
      "Bold 1980s geometric style",
      "ColorBrewer Dark2 for accessibility"
    ),
    background = c(
      "light",
      "white",
      "gray",
      "dark",
      "white",
      "white",
      "black",
      "white",
      "white",
      "white",
      "white"
    ),
    stringsAsFactors = FALSE
  )
  
  styles
}

#' Apply random network style
#'
#' Randomly selects and returns one of the available network styles
#'
#' @param seed Optional seed for reproducibility
#' @return A style list that can be passed to plot()
#' @export

style_random <- function(seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  styles <- c(
    "style_budapest", "style_economist", "style_538",
    "style_solarized", "style_stata", "style_nature", 
    "style_cyberpunk", "style_tufte", "style_pastel",
    "style_retro80s", "style_dark2"
  )
  
  selected <- sample(styles, 1)
  message(paste("Using", selected))
  
  # Call the selected style function
  do.call(selected, list())
}