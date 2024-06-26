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
