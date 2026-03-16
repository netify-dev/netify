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
			edge_color = "#F4B5BD", # soft pink
			node_fill = "#9A8C98", # purple
			node_color = "#C9ADA7", # light purple border
			edge_alpha = 0.6,
			curve_edges = FALSE
		),
		economist = list(
			edge_color = "#E3120B", # Economist red
			node_fill = "#6BABEB", # light blue
			node_color = "#01621E", # dark green
			edge_alpha = 0.7,
			curve_edges = FALSE
		),
		cyberpunk = list(
			edge_color = "#FF006E", # neon pink
			node_fill = "#00F5FF", # cyan
			node_color = "#FF006E", # pink border
			edge_alpha = 0.8,
			curve_edges = FALSE
		),
		nature = list(
			edge_color = "#003F7F", # dark blue
			node_fill = "#ED6D35", # orange
			node_color = "#003F7F", # dark blue border
			edge_alpha = 0.6,
			curve_edges = FALSE
		),
		pastel = list(
			edge_color = "#BB9CC0", # lavender
			node_fill = "#9EDAC0", # mint green
			node_color = "#FEB1D3", # pink border
			edge_alpha = 0.7,
			curve_edges = FALSE
		),
		solarized = list(
			edge_color = "#B58900", # yellow
			node_fill = "#268BD2", # blue
			node_color = "#CB4B16", # orange
			edge_alpha = 0.5,
			curve_edges = FALSE
		),
		viridis = list(
			edge_color = "#440154", # dark purple
			node_fill = "#21908C", # teal
			node_color = "#FDE725", # yellow
			edge_alpha = 0.6,
			curve_edges = FALSE
		),
		grayscale = list(
			edge_color = "#666666", # medium gray
			node_fill = "#333333", # dark gray
			node_color = "#000000", # black
			edge_alpha = 0.5,
			curve_edges = FALSE
		),
		sunset = list(
			edge_color = "#FF6B6B", # coral
			node_fill = "#FFE66D", # yellow
			node_color = "#4ECDC4", # turquoise
			edge_alpha = 0.7,
			curve_edges = FALSE
		),
		ocean = list(
			edge_color = "#006994", # deep blue
			node_fill = "#00A8CC", # light blue
			node_color = "#0C7B93", # medium blue
			edge_alpha = 0.6,
			curve_edges = FALSE
		)
	)

	# return the selected palette or NULL if not found
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
	c(
		"budapest", "economist", "cyberpunk", "nature", "pastel",
		"solarized", "viridis", "grayscale", "sunset", "ocean"
	)
}
