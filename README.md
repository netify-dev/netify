# netify

## Overview <img src="[https://github.com/netify-dev/netify/man/figures/hex.png](https://raw.githubusercontent.com/netify-dev/netify/main/figures/hex.png)" align = "right" alt="hex" width="200px">

Netify makes networks!

You supply data and `netify` transforms it into easy to work with network data.  The goal of netify is to provide R functions that simplify and facilitate common tasks related to network creation, summary, visualization, and modeling. Although our package was built with social scientists (especially peace science scholars) in mind, anyone can use it!

## Installation

      install.packages("netify")

or via devtools + github

      # install devtools
      if (!require(devtools)) {
        install.packages("devtools")
      }
      library(devtools)

      install_github("netify-dev/netify")


## Usage

See our `netify_introduction` for more information. To get started, supply a dyadic data set (or edgelist) to netify. You can add information such as nodal or dyadic covariates, time, as well as specify weights for the edges of the network. For example, to generate a longitudional, directed and weighted network use the code below:

      library(netify)
      data(icews)

      icews_conflict <- netify(
        dyad_data=icews,
        actor1='i', actor2='j',
        time='year',
        symmetric=FALSE, weight='matlConf')

      icews_conflict

## Contributors 

Cassy Dorff (Vanderbilt University) and Shahryar Minhas (Michigan State University) are co-PI's on the collaborative NSF grant funding this work, as such, they acknowledge support from National Science Foundation (NSF) Awards #2017162 and #2017180. Additional contributors include Ha Eun Choi (Ph.D. Candidate, Michigan State University) and Colin Henry (Ph.D. Candidate, Vanderbilt University). 
