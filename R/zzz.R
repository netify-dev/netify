# register S3 methods for external generics without hard dependencies

.onLoad <- function(libname, pkgname) {
	pkg_ns <- topenv(parent.frame())

	# broom generics (tidy / glance)
	if (requireNamespace("generics", quietly = TRUE)) {
		registerS3method("tidy", "netify", tidy.netify,
			envir = asNamespace("generics"))
		registerS3method("glance", "netify", glance.netify,
			envir = asNamespace("generics"))
	}
	# tibble as_tibble dispatch
	if (requireNamespace("tibble", quietly = TRUE)) {
		registerS3method("as_tibble", "netify", as_tibble.netify,
			envir = asNamespace("tibble"))
		registerS3method("as_tibble", "netify_comparison",
			as_tibble.netify_comparison,
			envir = asNamespace("tibble"))
	}

	# helper that registers as.network.netify against the network namespace
	register_as_network <- function() {
		if (exists("as.network.netify", envir = pkg_ns, inherits = FALSE)) {
			registerS3method("as.network", "netify",
				get("as.network.netify", envir = pkg_ns),
				envir = asNamespace("network"))
		}
	}

	# statnet as.network dispatch: register now if network is installed,
	# and also arm a hook so a later install/load of network still wires up.
	if (requireNamespace("network", quietly = TRUE)) {
		register_as_network()
	} else {
		setHook(packageEvent("network", "onLoad"),
			function(...) register_as_network())
	}

	# helper that registers as.igraph.netify against the igraph namespace
	register_as_igraph <- function() {
		if (exists("as.igraph.netify", envir = pkg_ns, inherits = FALSE)) {
			registerS3method("as.igraph", "netify",
				get("as.igraph.netify", envir = pkg_ns),
				envir = asNamespace("igraph"))
		}
	}

	# igraph as.igraph dispatch: register now if igraph is installed,
	# and also arm a hook so a later install/load of igraph still wires up.
	if (requireNamespace("igraph", quietly = TRUE)) {
		register_as_igraph()
	} else {
		setHook(packageEvent("igraph", "onLoad"),
			function(...) register_as_igraph())
	}
}
