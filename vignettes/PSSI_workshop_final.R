# Netify Workshop PSSI
# Last Updated 30 October 2024
# Script authors: Dorff, Keltner, Minhas

#------------------------------------------------------------------
# SET UP
#------------------------------------------------------------------

# install devtools + netify
if (!require(devtools)) {
  install.packages("devtools")
}
library(devtools)

install_github("netify-dev/netify")

library(netify)

# ex for installing extra packages for this vignette
if(!'tidyverse' %in% rownames(installed.packages())){
  install.packages('tidyverse', repos='https://cloud.r-project.org') }
if(!'peacesciencer' %in% rownames(installed.packages())){
  install.packages('peacesciencer', repos='https://cloud.r-project.org') }

# or load necessary packages for this vignette
library(peacesciencer)
library(tidyverse)
library(readxl)
library(ggplot2)
library(igraph)
library(patchwork)
library(ergm)
library(ggpubr)

#------------------------------------------------------------------
# Case examples using UCDP
# - UCDP GED (Mexican Case)
#     - Basic plots
#     - Weighted graphs
#     - Time varying variable
#     - Cross sectional graphs
# - UCDP GED (Myanmar Case)
#     - Binary variables 
#     - Bipartite networks
#------------------------------------------------------------------

# STEP 1: Basic aggregated network

# load GED data
UCDP_GED <- read_excel("GEDEvent_v24_1.xlsx")
mexico <- subset(UCDP_GED, country == "Mexico")

# make 1st net: default to number of events
mex_network <- netify(
  dyad_data = mexico,
  actor1 = 'side_a',
  actor2 = 'side_b',
  symmetric = TRUE,
  sum_dyads = TRUE,
  diag_to_NA = TRUE,
  missing_to_zero = TRUE
)

# summary statistics graph level
summary(mex_network)

# summary statistics actor level
get_stats<-summary_actor(mex_network)
plot_actor_stats(get_stats)

# visualization
plot(mex_network,
     add_text = TRUE)

# viz with less names
# select 10 random indices from 1 to the length of select_names
select_names <- rownames(mex_network)

set.seed(12345)
random_indices <- sample(length(select_names), 10)

# select 10 random names using the random indices
random_names <- select_names[random_indices]

plot(mex_network,
     add_text = TRUE)

plot(mex_network,
     select_text = random_names,
     select_text_display = random_names)

#------------------------------------------------------------------
# Weighted network
#------------------------------------------------------------------

# weight using civilian deaths variable
mex_network_civ <- netify(
  dyad_data = mexico,
  actor1 = 'side_a',
  actor2 = 'side_b',
  weight = 'deaths_civilians',
  symmetric = TRUE,
  sum_dyads = TRUE,
  diag_to_NA = TRUE,
  missing_to_zero = TRUE
)

# show summary and plot for weighted graph
summary(mex_network_civ)

# plot, see that color gradient of edge depends on weight variable
plot(mex_network_civ)

plot(mex_network_civ,
     add_text = TRUE)

plot(mex_network_civ,
     select_text = random_names,
     select_text_display = random_names)

# check this ex w/ SM
# community detection
i_opt_memb = function(x){
  ig = prep_for_igraph(x)
  memb = igraph::cluster_optimal(ig)$membership
  return(memb)
}

# add to summary
sum_mex <- summary_actor(
  mex_network_civ, 
  other_stats = list(i_opt_memb=i_opt_memb))

# create size by source count variable for add_nodal
size_by_source_count <- mexico |>
  pivot_longer(cols = c(side_a, side_b), 
               names_to = "actor_type", 
               values_to = "actor") |>
  group_by(actor) |>
  summarize(size_by_source_count = sum(number_of_sources, na.rm = TRUE)) |>
  ungroup()

# add size by source count as a nodal variable
mex_network_civ <- add_nodal(
  netlet = mex_network_civ,
  node_data = size_by_source_count,
  actor = 'actor',
  node_vars = c('size_by_source_count')
)

# Not the prettiest example (SM)
# node color/size based on sources
# edge color based on weight
plot(mex_network_civ, 
     point_size_var = 'size_by_source_count',
     point_color_var = 'size_by_source_count'
)

# only label points with number of sources > 1000 
# filter
large_nodes <- subset(size_by_source_count, 
                      size_by_source_count >= 1000)$actor

# plot (color gradient based on num sources)
plot(
  mex_network_civ,
  point_color_var = 'size_by_source_count',         
  select_text = large_nodes,                   
  select_text_display = large_nodes         
)

#------------------------------------------------------------------
# longitudinal network
#------------------------------------------------------------------

mex_network_long <- netify(
  dyad_data = mexico,
  actor1 = 'side_a',
  actor2 = 'side_b',
  time = 'year',      
  weight = 'deaths_civilians',
  symmetric = TRUE,
  sum_dyads = TRUE,
  diag_to_NA = TRUE,
  missing_to_zero = TRUE
)  

# summary stats for network_civ_long actors

# actor level summary stats
# distribution of stats for every year & every actor 
summary_actor_mex <- summary_actor(mex_network_long)

# density plot across all actors
plot_actor_stats(
  summary_actor_mex,
  across_actor= TRUE,
)

# zoom in on specific stat (ex closeness)
plot_actor_stats(
  summary_actor_mex,
  across_actor = TRUE,
  specific_stats='closeness'
)

# create a data frame for plotting
# SM what if one of the years has missing data? seems to break?
plot.netify(mex_network_long,
            static_actor_positions = TRUE,
            remove_isolates = FALSE)

# good moment to use subset_netlet
mex_network_short <- subset_netlet(mex_network_long,
                                   when_to_subset = c('2006','2008', 
                                                      '2010', '2012')
                                    )

plot(mex_network_short)


#------------------------------------------------------------------
# Myanmar UCDP GED dataset
#------------------------------------------------------------------

# Load the Myanmar GED dataset
myanmar <- subset(UCDP_GED, country == "Myanmar (Burma)")

# Create the network object for myanmar data
my_network <- netify(
  dyad_data = myanmar,
  actor1 = 'side_a',
  actor2 = 'side_b',
  symmetric = TRUE,
  sum_dyads = TRUE,
  diag_to_NA = TRUE,
  missing_to_zero = TRUE
)

# summary of network object
summary(my_network)

# weight using a variable death civilians
my_network_civ <- netify(
  dyad_data = myanmar,
  actor1 = 'side_a',
  actor2 = 'side_b',
  weight = 'deaths_civilians',
  symmetric = TRUE,
  sum_dyads = TRUE,
  diag_to_NA = TRUE,
  missing_to_zero = TRUE
)

# basic plot with labels
plot(my_network_civ,
     add_text = TRUE)

# data addition as EXAMPLES only.
# define actor categories for nodal attributes
gov_actors <- c("Government of Myanmar (Burma)", "BMA", "NUG")
nongov_actors <- c(
  "KNU", "MTA", "DKBA", "MDA", "RCSS", "Buddhists (Myanmar)", "ARSA",
  "Civilians", "KNPP", "KIO", "RSO", "UWSA", "NMSP", "ABSDF", "NSCN-K",
  "PSLF", "God's Army", "MDA - LM", "CNF", "Muslims (Myanmar)", "SSPP",
  "DKBA 5", "MNDAA", "ULA"
)
ethnic_actors <- c(
  "KNU", "RCSS", "KIO", "UWSA", "NMSP", "KNPP", "NSCN-K", "CNF", "PSLF", "SSPP",
  "MTA", "DKBA", "MDA", "RSO", "God's Army", "MDA - LM", "DKBA 5", "MNDAA", "ULA"
)

ideological_actors <- c(
  "Government of Myanmar (Burma)", "NUG", "ARSA", "ABSDF", "Civilians",
  "Buddhists (Myanmar)", "Muslims (Myanmar)", "BMA"
)

armed_actors <- c(
  "KNU", "MTA", "DKBA", "MDA", "RCSS", "ARSA", "NMSP", "KNPP", "KIO", "RSO",
  "UWSA", "ABSDF", "NSCN-K", "PSLF", "God's Army", "DKBA 5", "MNDAA", "ULA",
  "CNF", "SSPP", "NUG", "MDA - LM"
)

non_armed_actors <- c(
  "Government of Myanmar (Burma)", "BMA", "Civilians", "Buddhists (Myanmar)", "Muslims (Myanmar)"
)

# create nodal data for attributes
actors <- unique(c(myanmar$side_a, myanmar$side_b))
node_data <- data.frame(
  actor = actors,
  gov_or_nongov = ifelse(actors %in% gov_actors, 1, 
                         ifelse(actors %in% nongov_actors, 0, NA)),
  ethnic_or_ideological = ifelse(actors %in% ethnic_actors, 1, 
                                 ifelse(actors %in% ideological_actors, 0, NA)),
  armed_or_not = ifelse(actors %in% armed_actors, 1, 
                        ifelse(actors %in% non_armed_actors, 0, NA))
)

node_data <- node_data |>
  mutate_all(as.factor)

# add each nodal attribute to the network
my_network_civ <- add_nodal(my_network_civ, 
                            node_data = node_data, 
                            actor = 'actor', 
                            node_vars = 'gov_or_nongov')
my_network_civ <- add_nodal(my_network_civ, 
                            node_data = node_data, 
                            actor = 'actor', 
                            node_vars = 'ethnic_or_ideological')
my_network_civ <- add_nodal(my_network_civ, 
                            node_data = node_data, 
                            actor = 'actor', 
                            node_vars = 'armed_or_not')

# plot the network with specified attributes
plot(
  my_network_civ,
  title = "Myanmar Network",
  point_color_var = "gov_or_nongov", 
  layout = "circle",
  add_text = TRUE                           
)

plot(
  my_network_civ,
  title = "Myanmar Network",
  point_color_var = "ethnic_or_ideological", 
  layout = "circle",
  add_text = TRUE                           
)

#------------------------------------------------------------------
# Creating a bipartite network based on religious affilition
#------------------------------------------------------------------

# Define Buddhist-leaning and Christian-leaning actors
buddhist_actors <- c("PSLF", "DKBA", "God's Army", "Government of Myanmar (Burma)")   # Add any other Buddhist-leaning actors here if applicable
christian_actors <- c("KNU", "KNPP", "KIO", "MDA", "NSCN-K", "CNF")

# combine selected actors
selected_actors <- c(buddhist_actors, christian_actors)

# subset to only include interactions involving Buddhist or Christian actors
myanmar_bipartite <- subset(myanmar, side_a %in% selected_actors & side_b %in% selected_actors)

# debug step
if (nrow(myanmar_bipartite) == 0) {
  stop("no data for the specified Buddhist-Christian interactions.")
}

# keep bipartite structure
myanmar_bipartite$actor_type_a <- ifelse(myanmar_bipartite$side_a %in% buddhist_actors, "Buddhist", "Christian")
myanmar_bipartite$actor_type_b <- ifelse(myanmar_bipartite$side_b %in% buddhist_actors, "Buddhist", "Christian")

# create bipartite network
# assume no direct conflict between actors 
# of the same religious / quasi religious affiliation
bipartite_network <- netify(
  dyad_data = myanmar_bipartite,
  actor1 = 'side_a',
  actor2 = 'side_b',
  mode = 'bipartite',
  weight = 'deaths_civilians', 
  symmetric = FALSE,           
  sum_dyads = TRUE,
  diag_to_NA = TRUE,
  missing_to_zero = TRUE
)

# not working (SM?)
# plot bipartite network with labels
plot(
  bipartite_network,
  title = "Bipartite Network: Buddhist vs Christian Actors in Myanmar",
  add_text = TRUE,
  layout = "bipartite"
)

