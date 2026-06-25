#' Apply style to netify plot
#'
#' helper function to properly apply styles to netify plots
#'
#' @param netlet a netify object
#' @param style_fun a style function (e.g., style_rose)
#' @param ... additional plot parameters
#' @return a ggplot object
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export

plot_with_style <- function(netlet, style_fun, ...) {
	style_params <- style_fun()
	additional_params <- list(...)

	# additional params override style params
	all_params <- c(style_params, additional_params)

	do.call(plot, c(list(x = netlet), all_params))
}

#' preset style for bipartite networks
#'
#' a complete visual style optimized for displaying bipartite networks
#' with two distinct node sets
#'
#' @return list of plot arguments
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export

style_bipartite_network <- function() {
	list(
		# edges
		edge_color = "#7F8C8D",
		edge_alpha = 0.4,

		# combine with manual shape/color by node type at the call site
		point_size = 3,
		point_stroke = 1,

		# no curves for clarity
		curve_edges = FALSE,

		# often want labels for bipartite
		add_text = TRUE,
		text_size = 2.5,
		check_overlap = TRUE,

		# use bipartite layout
		layout = "bipartite"
	)
}

#' preset style for temporal/longitudinal networks
#'
#' a complete visual style optimized for displaying networks over time
#'
#' @return list of plot arguments
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export

style_temporal_network <- function() {
	list(
		# subtle edges
		edge_color = "#95A5A6",
		edge_alpha = 0.3,
		edge_linewidth = 0.5,

		# nodes that pop
		point_fill = "#3498DB",
		point_color = "#2980B9",
		point_size = 2,

		# static positions for comparison
		static_actor_positions = TRUE,

		# no curves for clarity across time
		curve_edges = FALSE,

		# minimal style works well with facets
		use_style_netify = TRUE
	)
}

#' rose network style
#'
#' a network style with soft rose, purple, and gold colors.
#'
#' @return list of plot arguments
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export

style_rose <- function() {
	list(
		# soft pink edges
		edge_color = "#F4B5BD",
		edge_alpha = 0.6,
		edge_linewidth = 0.8,

		# nodes in purple and gold
		point_fill = "#9A8C98",
		point_color = "#C9ADA7",
		point_stroke = 1.5,
		point_shape = 21,

		# curved edges for whimsical feel
		curve_edges = FALSE,
		edge_curvature = 0.3,

		# labels in dark purple
		text_color = "#4A4E69",
		text_size = 3,

		# light background
		use_style_netify = FALSE
	)
}

#' red and blue network style
#'
#' a network style with red accents and clean lines.
#'
#' @return list of plot arguments
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export

style_red_blue <- function() {
	list(
		# red edges
		edge_color = "#E3120B",
		edge_alpha = 0.7,
		edge_linewidth = 0.5,

		# light blue nodes
		point_fill = "#6BABEB",
		point_color = "#01621E", # dark green border
		point_stroke = 1,
		point_size = 2.5,
		point_shape = 21,

		# no curves - clean geometric look
		curve_edges = FALSE,

		# clean, minimal text
		text_family = "sans",
		text_size = 3,
		text_color = "#000000",

		# white background
		use_style_netify = TRUE
	)
}

#' orange and teal network style
#'
#' a network style with gray background and bright accents.
#'
#' @return list of plot arguments
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export

style_orange_teal <- function() {
	list(
		# orange edges
		edge_color = "#FC7634",
		edge_alpha = 0.6,
		edge_linewidth = 1,

		# teal nodes
		point_fill = "#30A9D9",
		point_color = "#2A2A2A",
		point_stroke = 0.5,
		point_size = 3,
		point_shape = 21,

		# straight edges
		curve_edges = FALSE,

		# bold text
		text_size = 3.5,
		text_color = "#2A2A2A",
		text_fontface = 2, # bold

		# gray background (apply manually)
		use_style_netify = FALSE
	)
}

#' solarized-inspired network style
#'
#' a network style based on the popular solarized color scheme,
#' optimized for reduced eye strain
#'
#' @return list of plot arguments
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export

style_solarized <- function() {
	list(
		# base yellow edges
		edge_color = "#B58900",
		edge_alpha = 0.5,
		edge_linewidth = 0.8,

		# blue nodes with orange border
		point_fill = "#268BD2",
		point_color = "#CB4B16",
		point_stroke = 1,
		point_size = 2.5,
		point_shape = 21,

		# slight curves
		curve_edges = FALSE,
		edge_curvature = 0.15,

		# readable text
		text_color = "#586E75",
		text_size = 3,

		# dark background (apply manually)
		use_style_netify = FALSE
	)
}

#' navy and maroon network style
#'
#' a network style with navy blue and maroon colors.
#'
#' @return list of plot arguments
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export

style_navy_maroon <- function() {
	list(
		# navy blue edges
		edge_color = "#1A476F",
		edge_alpha = 0.8,
		edge_linewidth = 0.6,

		# maroon nodes
		point_fill = "#90353B",
		point_color = "#1A476F",
		point_stroke = 1,
		point_size = 2,
		point_shape = 21,

		# no curves - traditional feel
		curve_edges = FALSE,

		# classic serif text
		text_family = "serif",
		text_size = 3,
		text_color = "#000000",
		use_style_netify = TRUE
	)
}

#' scientific blue network style
#'
#' a network style with clean blue and orange colors.
#'
#' @return list of plot arguments
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export

style_scientific_blue <- function() {
	list(
		# dark blue edges
		edge_color = "#003F7F",
		edge_alpha = 0.6,
		edge_linewidth = 0.7,

		# orange-red nodes
		point_fill = "#ED6D35",
		point_color = "#003F7F",
		point_stroke = 0.8,
		point_size = 2.2,
		point_shape = 21,

		# clean straight lines
		curve_edges = FALSE,

		# professional text
		text_size = 2.8,
		text_color = "#000000",
		use_style_netify = TRUE
	)
}

#' cyberpunk-inspired network style
#'
#' a futuristic network style with neon colors on dark background
#'
#' @return list of plot arguments
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export

style_cyberpunk <- function() {
	list(
		# neon pink edges
		edge_color = "#FF006E",
		edge_alpha = 0.8,
		edge_linewidth = 1,

		# cyan nodes
		point_fill = "#00F5FF",
		point_color = "#FF006E",
		point_stroke = 1.5,
		point_size = 2.5,
		point_shape = 21,

		# angular feel
		curve_edges = FALSE,

		# glowing text effect (white text)
		text_color = "#FFFFFF",
		text_size = 3,

		# dark background needed
		use_style_netify = FALSE
	)
}

#' minimal tufte-inspired network style
#'
#' a minimalist network style inspired by edward tufte's
#' data visualization principles
#'
#' @return list of plot arguments
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export

style_tufte <- function() {
	list(
		# very light gray edges
		edge_color = "#CCCCCC",
		edge_alpha = 0.5,
		edge_linewidth = 0.3,

		# small black nodes
		point_fill = "#000000",
		point_color = "#000000",
		point_stroke = 0,
		point_size = 1.5,
		point_shape = 21,

		# no curves - clean
		curve_edges = FALSE,

		# minimal text
		text_size = 2.5,
		text_color = "#333333",

		# remove all chrome
		use_style_netify = TRUE
	)
}

#' pastel rainbow network style
#'
#' a soft, cheerful network style using pastel colors
#'
#' @return list of plot arguments
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export

style_pastel <- function() {
	list(
		# lavender edges
		edge_color = "#BB9CC0",
		edge_alpha = 0.7,
		edge_linewidth = 1.2,

		# mint green nodes
		point_fill = "#9EDAC0",
		point_color = "#FEB1D3", # pink border
		point_stroke = 1.5,
		point_size = 3,
		point_shape = 21,

		# soft curves
		curve_edges = FALSE,
		edge_curvature = 0.4,

		# soft gray text
		text_color = "#666666",
		text_size = 3,
		use_style_netify = TRUE
	)
}

#' retro 80s-inspired network style
#'
#' a network style channeling 1980s design aesthetics
#' with bold colors and geometric shapes
#'
#' @return list of plot arguments
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export

style_retro80s <- function() {
	list(
		# hot pink edges
		edge_color = "#FF1493",
		edge_alpha = 0.9,
		edge_linewidth = 1.5,

		# electric blue nodes
		point_fill = "#00CED1",
		point_color = "#FFD700", # gold border
		point_stroke = 2,
		point_size = 3.5,
		point_shape = 24, # triangle

		# no curves - geometric
		curve_edges = FALSE,

		# bold text
		text_size = 4,
		text_color = "#FF1493",
		text_fontface = 2,
		use_style_netify = TRUE
	)
}

#' colorbrewer dark2-based network style
#'
#' a network style using colorbrewer's dark2 palette
#' for high contrast and accessibility
#'
#' @return list of plot arguments
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export

style_dark2 <- function() {
	list(
		# dark orange edges
		edge_color = "#D95F02",
		edge_alpha = 0.6,
		edge_linewidth = 0.8,

		# dark green nodes with purple border
		point_fill = "#1B9E77",
		point_color = "#7570B3",
		point_stroke = 1.2,
		point_size = 2.5,
		point_shape = 21,

		# slight curves
		curve_edges = FALSE,
		edge_curvature = 0.2,

		# dark text
		text_color = "#666666",
		text_size = 3,
		use_style_netify = TRUE
	)
}

#' crimson and silver network style
#'
#' a network style with sharp contrasts and sleek design.
#'
#' @return list of plot arguments
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export

style_crimson_silver <- function() {
	list(
		# sharp black edges
		edge_color = "#1C1C1C", # almost black
		edge_alpha = 0.9,
		edge_linewidth = 0.4, # thin and precise

		# crimson nodes with silver borders
		point_fill = "#8B0000", # dark red (blood)
		point_color = "#C0C0C0", # silver trim
		point_stroke = 0.8,
		point_size = 2,
		point_shape = 23, # diamond - sharp and agile

		# straight edges
		curve_edges = FALSE,

		# precise text
		text_color = "#8B0000",
		text_size = 2.5,
		use_theme_netify = TRUE
	)
}

#' green and gold network style
#'
#' a network style with balanced green, gold, and blue colors.
#'
#' @return list of plot arguments
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export

style_green_gold <- function() {
	list(
		# blue edges
		edge_color = "#4169E1", # royal blue
		edge_alpha = 0.7,
		edge_linewidth = 0.8,

		# green nodes with gold borders
		point_fill = "#228B22", # forest green
		point_color = "#FFD700", # gold trim
		point_stroke = 1.2,
		point_size = 2.5,
		point_shape = 21, # circle - balanced

		# slight curves
		curve_edges = FALSE,
		edge_curvature = 0.2,

		# scholarly text
		text_color = "#4169E1",
		text_size = 3,
		use_theme_netify = TRUE
	)
}

#' slate and silver network style
#'
#' a network style with steel gray and silver colors.
#'
#' @return list of plot arguments
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export

style_slate_silver <- function() {
	list(
		# steel gray edges
		edge_color = "#708090", # slate gray
		edge_alpha = 0.8,
		edge_linewidth = 1,

		# white/silver nodes with dark borders
		point_fill = "#F5F5F5", # white wolf
		point_color = "#2F4F4F", # dark slate gray
		point_stroke = 1.5,
		point_size = 3,
		point_shape = 21, # circle

		# moderate curves
		curve_edges = FALSE,
		edge_curvature = 0.3,

		# clear text
		text_color = "#2F4F4F",
		text_size = 3,
		text_fontface = 2, # bold

		use_theme_netify = TRUE
	)
}

#' bronze block network style
#'
#' a network style with bold, substantial visuals.
#'
#' @return list of plot arguments
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export

style_bronze_block <- function() {
	list(
		# heavy brown edges
		edge_color = "#8B4513", # saddle brown
		edge_alpha = 0.9,
		edge_linewidth = 2, # thick and heavy

		# dark nodes with bronze borders
		point_fill = "#2F2F2F", # almost black
		point_color = "#CD7F32", # bronze trim
		point_stroke = 2,
		point_size = 4, # large and imposing
		point_shape = 22, # square - sturdy

		# no curves - straightforward
		curve_edges = FALSE,

		# heavy text
		text_color = "#8B4513",
		text_size = 4,
		text_fontface = 2, # bold

		use_theme_netify = TRUE
	)
}

#' lime and magenta network style
#'
#' a network style with bright green and magenta contrast.
#'
#' @return list of plot arguments
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export

style_lime_magenta <- function() {
	list(
		# lime green edges
		edge_color = "#32CD32", # lime green
		edge_alpha = 0.6,
		edge_linewidth = 0.7,

		# purple nodes with green borders
		point_fill = "#8B008B", # dark magenta
		point_color = "#00FF00", # bright green
		point_stroke = 1,
		point_size = 2.5,
		point_shape = 24, # triangle up - exotic

		# twisted curves
		curve_edges = FALSE,
		edge_curvature = 0.5, # more curved - mysterious

		# mysterious text
		text_color = "#8B008B",
		text_size = 2.8,
		use_theme_netify = TRUE
	)
}

#' black and yellow network style
#'
#' a network style with dark green, black, and yellow colors.
#'
#' @return list of plot arguments
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export

style_black_yellow <- function() {
	list(
		# dark green edges
		edge_color = "#006400", # dark green
		edge_alpha = 0.5,
		edge_linewidth = 0.6,

		# black nodes with yellow borders
		point_fill = "#000000", # black
		point_color = "#FFFF00", # yellow
		point_stroke = 0.8,
		point_size = 1.8,
		point_shape = 23, # diamond

		# curved edges
		curve_edges = FALSE,
		edge_curvature = 0.4,

		# subtle text
		text_color = "#006400",
		text_size = 2.5,
		use_theme_netify = TRUE
	)
}

#' sunburst network style
#'
#' a playful style with energetic yellows and bold outlines.
#'
#' @return list of plot arguments
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export
style_sunburst <- function() {
	list(
		edge_color = "#FFD700", # yellow
		edge_alpha = 0.8,
		edge_linewidth = 1,
		point_fill = "#FFFACD", # light yellow node fill
		point_color = "#DAA520", # goldenrod border
		point_stroke = 1.2,
		point_size = 3,
		point_shape = 21, # circle

		curve_edges = FALSE,
		edge_curvature = 0.25,
		text_color = "#555555",
		text_size = 3,
		use_style_netify = TRUE
	)
}

#' racing blue network style
#'
#' a sleek style with clean lines, blue accents, and strong contrast.
#'
#' @return list of plot arguments
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export
style_racing_blue <- function() {
	list(
		edge_color = "#1F75FE", # racing blue
		edge_alpha = 0.7,
		edge_linewidth = 1.2,
		point_fill = "#FFFFFF", # white silks
		point_color = "#1F75FE", # blue trim
		point_stroke = 2,
		point_size = 3.5,
		point_shape = 22, # square marker

		curve_edges = FALSE, # direct and fast

		text_color = "#1F1F1F",
		text_size = 3.2,
		text_fontface = 2, # bold for strength

		use_style_netify = TRUE
	)
}



#' list available network styles
#'
#' shows all available preset network styles with descriptions
#'
#' @return a data frame with style names and descriptions
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export

list_network_styles <- function() {
	styles <- data.frame(
		style = c(
			"style_rose",
			"style_red_blue",
			"style_orange_teal",
			"style_solarized",
			"style_navy_maroon",
			"style_scientific_blue",
			"style_cyberpunk",
			"style_tufte",
			"style_pastel",
			"style_retro80s",
			"style_dark2",
			"style_crimson_silver",
			"style_green_gold",
			"style_slate_silver",
			"style_bronze_block",
			"style_lime_magenta",
			"style_black_yellow",
			"style_sunburst",
			"style_racing_blue"
		),
		description = c(
			"Soft rose, purple, and gold",
			"Red and blue accents on a clean background",
			"Orange and teal on gray",
			"Solarized color scheme for reduced eye strain",
			"Classic navy and maroon",
			"Scientific blue and orange",
			"Neon cyberpunk with pink and cyan",
			"Edward Tufte's minimalist approach",
			"Soft pastel colors for a gentle look",
			"Bold 1980s geometric style",
			"ColorBrewer Dark2 for accessibility",
			"Crimson and silver with sharp contrast",
			"Green, gold, and blue balance",
			"Slate and silver contrast",
			"Bronze and block shapes",
			"Lime and magenta contrast",
			"Black and yellow contrast",
			"Bright yellow sunburst",
			"Blue accents with strong contrast"
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
			"white",
			"white",
			"white",
			"white",
			"white",
			"white",
			"black",
			"gray",
			"white"
		),
		stringsAsFactors = FALSE
	)

	styles
}

#' apply random network style
#'
#' randomly selects and returns one of the available network styles
#'
#' @param seed optional seed for reproducibility
#' @return a style list that can be passed to plot()
#'
#' @author cassy dorff, shahryar minhas
#'
#' @export

style_random <- function(seed = NULL) {
	styles <- c(
		"style_rose", "style_red_blue", "style_orange_teal",
		"style_solarized", "style_navy_maroon", "style_scientific_blue",
		"style_cyberpunk", "style_tufte", "style_pastel",
		"style_retro80s", "style_dark2"
	)

	selected <- with_local_seed(seed, sample(styles, 1))
	cli::cli_inform("Using {.fn {selected}}")

	do.call(selected, list())
}
