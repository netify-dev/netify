% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ego_layout_functions.R
\name{remove_ego_edges}
\alias{remove_ego_edges}
\title{Remove ego-alter edges from ego network}
\usage{
remove_ego_edges(netlet)
}
\arguments{
\item{netlet}{An ego network created with ego_netify()}
}
\value{
A modified netify object with ego edges removed
}
\description{
This function removes all edges between the ego and alters in an ego network,
leaving only the alter-alter connections visible. This is useful for visualizing
the structure among alters without the clutter of ego connections.
}
\examples{
\dontrun{
ego_net <- ego_netify(my_network, ego = "Alice")
alter_only_net <- remove_ego_edges(ego_net)
plot(alter_only_net)
}
}
