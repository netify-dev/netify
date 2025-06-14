#' Get predefined color palette
#'
#' Returns a list of colors for edges and nodes based on palette name
#'
#' @param palette_name Character string naming the palette
#' @return List with edge_color, node_fill, node_color, and other styling elements
#' 
#' @keywords internal
#' @noRd

get_palette <- function(palette_name) {
  
  palettes <- list(
    budapest = list(
      edge_color = "#F4B5BD",      # Soft pink
      node_fill = "#9A8C98",       # Purple
      node_color = "#C9ADA7",      # Light purple border
      edge_alpha = 0.6,
      curve_edges = TRUE
    ),
    
    economist = list(
      edge_color = "#E3120B",      # Economist red
      node_fill = "#6BABEB",       # Light blue
      node_color = "#01621E",      # Dark green
      edge_alpha = 0.7,
      curve_edges = FALSE
    ),
    
    cyberpunk = list(
      edge_color = "#FF006E",      # Neon pink
      node_fill = "#00F5FF",       # Cyan
      node_color = "#FF006E",      # Pink border
      edge_alpha = 0.8,
      curve_edges = FALSE
    ),
    
    nature = list(
      edge_color = "#003F7F",      # Dark blue
      node_fill = "#ED6D35",       # Orange
      node_color = "#003F7F",      # Dark blue border
      edge_alpha = 0.6,
      curve_edges = FALSE
    ),
    
    pastel = list(
      edge_color = "#BB9CC0",      # Lavender
      node_fill = "#9EDAC0",       # Mint green
      node_color = "#FEB1D3",      # Pink border
      edge_alpha = 0.7,
      curve_edges = TRUE
    ),
    
    solarized = list(
      edge_color = "#B58900",      # Yellow
      node_fill = "#268BD2",       # Blue
      node_color = "#CB4B16",      # Orange
      edge_alpha = 0.5,
      curve_edges = TRUE
    ),
    
    viridis = list(
      edge_color = "#440154",      # Dark purple
      node_fill = "#21908C",       # Teal
      node_color = "#FDE725",      # Yellow
      edge_alpha = 0.6,
      curve_edges = FALSE
    ),
    
    grayscale = list(
      edge_color = "#666666",      # Medium gray
      node_fill = "#333333",       # Dark gray
      node_color = "#000000",      # Black
      edge_alpha = 0.5,
      curve_edges = FALSE
    ),
    
    sunset = list(
      edge_color = "#FF6B6B",      # Coral
      node_fill = "#FFE66D",       # Yellow
      node_color = "#4ECDC4",      # Turquoise
      edge_alpha = 0.7,
      curve_edges = TRUE
    ),
    
    ocean = list(
      edge_color = "#006994",      # Deep blue
      node_fill = "#00A8CC",       # Light blue
      node_color = "#0C7B93",      # Medium blue
      edge_alpha = 0.6,
      curve_edges = TRUE
    )
  )
  
  # Return the selected palette or NULL if not found
  if (palette_name %in% names(palettes)) {
    return(palettes[[palette_name]])
  } else {
    return(NULL)
  }
}

#' List available color palettes
#'
#' @return Character vector of available palette names
#' @export

list_palettes <- function() {
  c("budapest", "economist", "cyberpunk", "nature", "pastel", 
    "solarized", "viridis", "grayscale", "sunset", "ocean")
}