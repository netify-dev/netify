# netify

## Overview <img src="[https://github.com/netify-dev/netify/assets/1364446/3186de64-9917-4b29-a67e-7f88b25d0848](https://github.com/netify-dev/netify/assets/1364446/5843fca9-cb72-4ce4-a253-dd0c75eb3539)" align = "right" alt="hex" width="200px">

Netify makes networks!

You supply data and netify transforms it into easy to work with network data.  The goal of netify is to provide R functions that simplify and facilitate common tasks related to network creation, summary, visualization, and modeling. Although our package was built with social scientists (especially peace science scholars) in mind, anyone can use it!

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

