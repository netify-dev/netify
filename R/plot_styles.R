#' Apply style to netify plot
#'
#' Helper function to properly apply styles to netify plots
#'
#' @param netlet A netify object
#' @param style_fun A style function (e.g., style_budapest)
#' @param ... Additional plot parameters
#' @return A ggplot object
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export

plot_with_style <- function(netlet, style_fun, ...) {
	style_params <- style_fun()
	additional_params <- list(...)

	# additional params override style params
	all_params <- c(style_params, additional_params)

	do.call(plot, c(list(x = netlet), all_params))
}

#' Preset style for bipartite networks
#'
#' A complete visual style optimized for displaying bipartite networks
#' with two distinct node sets
#'
#' @return List of plot arguments
#'
#' @author Cassy Dorff, Shahryar Minhas
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

#' Preset style for temporal/longitudinal networks
#'
#' A complete visual style optimized for displaying networks over time
#'
#' @return List of plot arguments
#'
#' @author Cassy Dorff, Shahryar Minhas
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

#' Wes Anderson-inspired network style (Grand Budapest Hotel)
#'
#' A network style inspired by the pastel color palette of
#' Wes Anderson's Grand Budapest Hotel
#'
#' @return List of plot arguments
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export

style_budapest <- function() {
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

#' Economist-style network style
#'
#' A network style inspired by The Economist's distinctive
#' visual style with red accents and clean lines
#'
#' @return List of plot arguments
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export

style_economist <- function() {
	list(
		# signature Economist red for edges
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

#' FiveThirtyEight-inspired network style
#'
#' A network style inspired by FiveThirtyEight's data visualization
#' style with gray background and bright accents
#'
#' @return List of plot arguments
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export

style_538 <- function() {
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

#' Solarized-inspired network style
#'
#' A network style based on the popular Solarized color scheme,
#' optimized for reduced eye strain
#'
#' @return List of plot arguments
#'
#' @author Cassy Dorff, Shahryar Minhas
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

#' Stata-inspired network style
#'
#' A network style that mimics Stata's default graph colors
#' with navy blue and maroon
#'
#' @return List of plot arguments
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export

style_stata <- function() {
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

#' Nature journal-inspired network style
#'
#' A network style inspired by Nature journal's clean scientific
#' visualization style
#'
#' @return List of plot arguments
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export

style_nature <- function() {
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

#' Cyberpunk-inspired network style
#'
#' A futuristic network style with neon colors on dark background
#'
#' @return List of plot arguments
#'
#' @author Cassy Dorff, Shahryar Minhas
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

#' Minimal Tufte-inspired network style
#'
#' A minimalist network style inspired by Edward Tufte's
#' data visualization principles
#'
#' @return List of plot arguments
#'
#' @author Cassy Dorff, Shahryar Minhas
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

#' Pastel rainbow network style
#'
#' A soft, cheerful network style using pastel colors
#'
#' @return List of plot arguments
#'
#' @author Cassy Dorff, Shahryar Minhas
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

#' Retro 80s-inspired network style
#'
#' A network style channeling 1980s design aesthetics
#' with bold colors and geometric shapes
#'
#' @return List of plot arguments
#'
#' @author Cassy Dorff, Shahryar Minhas
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

#' ColorBrewer Dark2-based network style
#'
#' A network style using ColorBrewer's Dark2 palette
#' for high contrast and accessibility
#'
#' @return List of plot arguments
#'
#' @author Cassy Dorff, Shahryar Minhas
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

#' Witcher 3 Feline School network style
#'
#' A network style inspired by the Cat School armor from The Witcher 3.
#' Light, agile, with emphasis on critical strikes - translates to sharp
#' contrasts and sleek design.
#'
#' @return List of plot arguments
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export

style_witcher_feline <- function() {
	list(
		# sharp black edges like cat claws
		edge_color = "#1C1C1C", # almost black
		edge_alpha = 0.9,
		edge_linewidth = 0.4, # thin and precise

		# crimson nodes with silver borders
		point_fill = "#8B0000", # dark red (blood)
		point_color = "#C0C0C0", # silver trim
		point_stroke = 0.8,
		point_size = 2,
		point_shape = 23, # diamond - sharp and agile

		# no curves - direct strikes
		curve_edges = FALSE,

		# precise text
		text_color = "#8B0000",
		text_size = 2.5,
		use_theme_netify = TRUE
	)
}

#' Witcher 3 Griffin School network style
#'
#' A network style inspired by the Griffin School armor from The Witcher 3.
#' Balanced between signs and swordplay - moderate, magical aesthetics.
#'
#' @return List of plot arguments
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export

style_witcher_griffin <- function() {
	list(
		# magical blue edges
		edge_color = "#4169E1", # royal blue (Aard sign)
		edge_alpha = 0.7,
		edge_linewidth = 0.8,

		# green nodes with gold borders (magical balance)
		point_fill = "#228B22", # forest green
		point_color = "#FFD700", # gold trim
		point_stroke = 1.2,
		point_size = 2.5,
		point_shape = 21, # circle - balanced

		# slight curves for magical flow
		curve_edges = FALSE,
		edge_curvature = 0.2,

		# scholarly text
		text_color = "#4169E1",
		text_size = 3,
		use_theme_netify = TRUE
	)
}

#' Witcher 3 Wolven School network style
#'
#' A network style inspired by the Wolf School armor from The Witcher 3.
#' The most balanced school - Geralt's home school at Kaer Morhen.
#'
#' @return List of plot arguments
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export

style_witcher_wolven <- function() {
	list(
		# steel gray edges
		edge_color = "#708090", # slate gray (steel sword)
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
		text_fontface = 2, # bold like a wolf

		use_theme_netify = TRUE
	)
}

#' Witcher 3 Ursine School network style
#'
#' A network style inspired by the Bear School armor from The Witcher 3.
#' Heavy, tank-like armor - translates to bold, substantial visuals.
#'
#' @return List of plot arguments
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export

style_witcher_ursine <- function() {
	list(
		# heavy brown edges
		edge_color = "#8B4513", # saddle brown (bear fur)
		edge_alpha = 0.9,
		edge_linewidth = 2, # thick and heavy

		# dark nodes with bronze borders
		point_fill = "#2F2F2F", # almost black (bear)
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

#' Witcher 3 Manticore School network style (bonus)
#'
#' A network style inspired by the Manticore School armor from The Witcher 3.
#' Exotic, alchemical focus - poisonous and mysterious.
#'
#' @return List of plot arguments
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export

style_witcher_manticore <- function() {
	list(
		# poison green edges
		edge_color = "#32CD32", # lime green (toxicity)
		edge_alpha = 0.6,
		edge_linewidth = 0.7,

		# purple nodes with green borders (alchemical)
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

#' Witcher 3 Viper School network style (bonus)
#'
#' A network style inspired by the Viper School armor from The Witcher 3.
#' Lethal, assassination-focused - stealth and poison.
#'
#' @return List of plot arguments
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export

style_witcher_viper <- function() {
	list(
		# dark green edges (snake)
		edge_color = "#006400", # dark green
		edge_alpha = 0.5, # stealthy
		edge_linewidth = 0.6,

		# black nodes with yellow borders (snake pattern)
		point_fill = "#000000", # black
		point_color = "#FFFF00", # yellow (viper eyes)
		point_stroke = 0.8,
		point_size = 1.8, # small and deadly
		point_shape = 23, # diamond - fangs

		# sinuous curves
		curve_edges = FALSE,
		edge_curvature = 0.4,

		# subtle text
		text_color = "#006400",
		text_size = 2.5,
		use_theme_netify = TRUE
	)
}

#' Pokemon-style network style (Pikachu)
#'
#' A playful style inspired by Pikachu: energetic yellows, bold outlines, and lightning vibes
#'
#' @return List of plot arguments
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export
style_pokemon_pikachu <- function() {
	list(
		edge_color = "#FFD700", # electric yellow
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

#' Secretariat-style network style
#'
#' A powerful, sleek style inspired by Secretariat - clean lines, racing blues, and commanding contrast.
#'
#' @return List of plot arguments
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
#' @export
style_secretariat <- function() {
	list(
		edge_color = "#1F75FE", # racing blue
		edge_alpha = 0.7,
		edge_linewidth = 1.2,
		point_fill = "#FFFFFF", # white silks
		point_color = "#1F75FE", # blue trim
		point_stroke = 2,
		point_size = 3.5,
		point_shape = 22, # square - powerful base

		curve_edges = FALSE, # direct and fast

		text_color = "#1F1F1F",
		text_size = 3.2,
		text_fontface = 2, # bold for strength

		use_style_netify = TRUE
	)
}



#' List available network styles
#'
#' Shows all available preset network styles with descriptions
#'
#' @return A data frame with style names and descriptions
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
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
			"style_dark2",
			"style_witcher_feline",
			"style_witcher_griffin",
			"style_witcher_wolven",
			"style_witcher_ursine",
			"style_witcher_manticore",
			"style_witcher_viper",
			"style_pokemon_pikachu",
			"style_secretariat"
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
			"ColorBrewer Dark2 for accessibility",
			"Witcher Cat School - sharp and agile",
			"Witcher Griffin School - magical balance",
			"Witcher Wolf School - Geralt's balanced style",
			"Witcher Bear School - heavy and imposing",
			"Witcher Manticore School - alchemical and exotic",
			"Witcher Viper School - stealthy assassin",
			"Pokemon-inspired Pikachu style",
			"Secretariat the Horse that ran fast"
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

#' Apply random network style
#'
#' Randomly selects and returns one of the available network styles
#'
#' @param seed Optional seed for reproducibility
#' @return A style list that can be passed to plot()
#'
#' @author Cassy Dorff, Shahryar Minhas
#'
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
	cli::cli_inform("Using {.fn {selected}}")

	do.call(selected, list())
}
