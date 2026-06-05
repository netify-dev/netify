# Pipeline: netify to ergm (statnet)

ERGMs (Exponential Random Graph Models) are the workhorse of inferential
network analysis in the [statnet](https://statnet.org/) ecosystem. With
`netify`, build the network, attach attributes with
[`add_node_vars()`](https://netify-dev.github.io/netify/reference/add_node_vars.md)
/
[`add_dyad_vars()`](https://netify-dev.github.io/netify/reference/add_dyad_vars.md),
then convert to the `network` format that `ergm` expects with
[`to_statnet()`](https://netify-dev.github.io/netify/reference/netify_to_statnet.md).

This vignette covers:

1.  The cross-sectional pipeline (single network -\> single ergm fit)
2.  The longitudinal pipeline (per-time ergm fits)
3.  The multilayer pipeline (per-layer ergm fits –
    [`to_statnet()`](https://netify-dev.github.io/netify/reference/netify_to_statnet.md)
    now iterates layers automatically)
4.  Round-tripping ergm-simulated networks back into netify for
    descriptive checks

``` r

library(netify)
library(ggplot2)
library(network)
#> 
#> 'network' 1.20.0 (2026-02-06), part of the Statnet Project
#> * 'news(package="network")' for changes since last version
#> * 'citation("network")' for citation information
#> * 'https://statnet.org' for help, support, and other information
library(ergm)
#> 
#> 'ergm' 4.12.0 (2026-02-17), part of the Statnet Project
#> * 'news(package="ergm")' for changes since last version
#> * 'citation("ergm")' for citation information
#> * 'https://statnet.org' for help, support, and other information
#> 'ergm' 4 is a major update that introduces some backwards-incompatible
#> changes. Please type 'news(package="ergm")' for a list of major
#> changes.

data(icews)
```

## 1. cross-sectional pipeline

The simplest case: one snapshot, one model.

``` r

# build a single-year netify object with nodal and dyadic attributes
icews_2010 <- icews[icews$year == 2010, ]

verb_coop <- netify(
    icews_2010,
    actor1 = "i", actor2 = "j",
    symmetric = FALSE,
    weight   = "verbCoop",
    nodal_vars = c("i_polity2", "i_log_gdp"),
    dyad_vars  = "matlCoop"
)
#> ℹ `missing_to_zero` is set to "TRUE" (the default).
#> ! Missing dyads will be filled with zeros. For latent space or other
#>   statistical network models, structural zeros and missing data have different
#>   meanings. Set `missing_to_zero = FALSE` to preserve NAs if this distinction
#>   matters for your analysis.
#> This message is displayed once per session.

# convert to statnet 'network' object
sn_2010 <- to_statnet(verb_coop)
#> ! Nodal columns with "NA" detected: "i_polity2" and "i_log_gdp". Ergm terms
#>   like `nodecov()`/`nodematch()` will refuse to fit.
#> ℹ Use `drop_na_actors(net, cols = c('i_polity2', 'i_log_gdp'))` (or impute)
#>   before refitting.
#> ℹ Dyad covariates attached as per-edge attributes under "matlCoop_e" and as
#>   network-level matrices under their original names ("matlCoop").
#> ℹ For `ergm::edgecov()` use the matrix name (e.g. `edgecov('matlCoop')`); the
#>   "_e" per-edge attribute is for descriptive use such as edge styling.
#> This message is displayed once per session.
sn_2010
#>  Network attributes:
#>   vertices = 152 
#>   directed = TRUE 
#>   hyper = FALSE 
#>   loops = FALSE 
#>   multiple = FALSE 
#>   bipartite = FALSE 
#>   verbCoop: 152x152 matrix
#>   matlCoop: 152x152 matrix
#>   total edges= 9976 
#>     missing edges= 0 
#>     non-missing edges= 9976 
#> 
#>  Vertex attribute names: 
#>     i_log_gdp i_polity2 vertex.names 
#> 
#>  Edge attribute names not shown
```

[`to_statnet()`](https://netify-dev.github.io/netify/reference/netify_to_statnet.md)
carries nodal attributes through as vertex attributes and dyadic
attributes through as edge attributes.

### before you fit: three sanity checks

ERGMs fail in cryptic ways when the underlying network is malformed.
Three checks catch most “invalid output from statistic” headaches before
you ever call [`ergm()`](https://rdrr.io/pkg/ergm/man/ergm.html):

``` r

# 1. nas in nodal covariates referenced by nodecov / nodematch
nd <- attr(verb_coop, "nodal_data")
na_cols <- names(nd)[vapply(nd, function(c) any(is.na(c)), logical(1))]
na_cols <- setdiff(na_cols, c("actor", "time", "layer"))
na_cols
#> [1] "i_polity2" "i_log_gdp"

# 2. isolates -- ergm fits but some terms (e.g. gwdegree) degenerate
m <- get_raw(verb_coop)
bin <- (m != 0) & !is.na(m)
deg_total <- rowSums(bin) + colSums(bin)
isolates <- rownames(m)[deg_total == 0]
length(isolates)
#> [1] 0

# 3. symmetry: the netify flag must match how your model treats ties
attr(verb_coop, "symmetric")
#> [1] FALSE
```

If `na_cols` is non-empty, drop the affected actors with
`drop_na_actors(verb_coop, cols = na_cols)` or impute before converting.
Here we create a cleaned network before showing a model formula that
uses nodal covariates:

``` r

clean_cols <- attr(sn_2010, "netify_na_cols")
if (is.null(clean_cols)) clean_cols <- na_cols
if (!is.null(clean_cols) && length(clean_cols) > 0) {
    verb_coop_clean <- drop_na_actors(verb_coop, cols = clean_cols)
} else {
    verb_coop_clean <- verb_coop
}
#> ℹ Dropped 5 of 152 actors with NA covariates: "Afghanistan", "Bosnia And
#>   Herzegovina", "Djibouti", "Korea, Democratic People's Republic Of", and
#>   "Somalia".
#> This message is displayed once per session.

sn_2010_clean <- to_statnet(verb_coop_clean)
attr(sn_2010_clean, "netify_na_cols")
#> character(0)
```

Now fit an ergm against the cleaned network:

``` r

# note: this chunk is not evaluated by default to keep vignette build fast.
# replace eval = false with eval = true to actually run.
set.seed(6886)
m <- ergm(
    sn_2010_clean ~ edges +
        nodecov("i_polity2") +
        nodecov("i_log_gdp")
)
summary(m)
```

The full model above is not evaluated during vignette builds. The short
block below checks the
[`netify()`](https://netify-dev.github.io/netify/reference/netify.md)
-\>
[`to_statnet()`](https://netify-dev.github.io/netify/reference/netify_to_statnet.md)
-\> [`ergm()`](https://rdrr.io/pkg/ergm/man/ergm.html) handoff:

``` r

toy_edges <- data.frame(
    i = c("a", "a", "b", "c"),
    j = c("b", "c", "c", "d"),
    y = 1
)
toy_net <- suppressMessages(netify(
    toy_edges,
    actor1 = "i", actor2 = "j",
    weight = "y", symmetric = FALSE
))
toy_sn <- to_statnet(toy_net)
set.seed(6886)
invisible(capture.output(
    toy_fit <- ergm(toy_sn ~ edges, estimate = "MPLE", eval.loglik = FALSE)
))
#> Starting maximum pseudolikelihood estimation (MPLE):
#> Obtaining the responsible dyads.
#> Evaluating the predictor and response matrix.
#> Maximizing the pseudolikelihood.
#> Finished MPLE.
coef(toy_fit)
#>      edges 
#> -0.6931472
```

One common ERGM failure comes from missing nodal covariates:
`ergm::nodecov("i_polity2")` will refuse to fit if any vertex has an
`NA` polity score. Either subset to actors with complete covariates or
impute before passing to
[`to_statnet()`](https://netify-dev.github.io/netify/reference/netify_to_statnet.md).
When
[`to_statnet()`](https://netify-dev.github.io/netify/reference/netify_to_statnet.md)
sees NAs in nodal attributes, it records the affected column names on
the output object. The
[`drop_na_actors()`](https://netify-dev.github.io/netify/reference/drop_na_actors.md)
helper handles this for cross-sectional, longitudinal, and bipartite
netlets. Use the same approach before formulas that reference nodal
attributes with `ergm::nodecov()` or `ergm::nodematch()`.

### dyadic edge covariates: the `_e` suffix

Any dyadic covariate you passed to
[`netify()`](https://netify-dev.github.io/netify/reference/netify.md)
(e.g. `dyad_vars = "matlCoop"`) is attached to the resulting `network`
object in two places:

- as a network-level attribute under its original name (the full `n x n`
  matrix), accessible via
  `network::get.network.attribute(sn, "matlCoop")`, and
- as a per-edge attribute under `<var>_e` (here, `matlCoop_e`),
  populated only on edges that actually exist.

The trailing `_e` disambiguates the per-edge edgelist from the
network-level matrix. For `ergm::edgecov()`, pass the **original
(matrix) name** – `edgecov()` resolves its argument as a network-level
matrix attribute, so the `_e` per-edge alias will not work there:

``` r

m <- ergm(sn_2010 ~ edges + edgecov("matlCoop"))
```

The `_e` per-edge attribute is exposed for descriptive uses (for
example, coloring edges by covariate in
[`network::plot.network`](https://rdrr.io/pkg/network/man/plot.network.html)).
[`to_statnet()`](https://netify-dev.github.io/netify/reference/netify_to_statnet.md)
emits a one-shot inform listing both forms the first time it attaches
dyadic covariates.

Goodness-of-fit, mcmc.diagnostics, and other postestimation tools live
in `ergm` itself.

## 2. longitudinal pipeline (per-time fits)

For a longitudinal netify,
[`to_statnet()`](https://netify-dev.github.io/netify/reference/netify_to_statnet.md)
returns a named list – one `network` object per time period:

``` r

verb_longit <- netify(
    icews[icews$year %in% 2010:2012, ],
    actor1 = "i", actor2 = "j", time = "year",
    symmetric = FALSE,
    weight = "verbCoop",
    nodal_vars = "i_polity2"
)

sn_list <- to_statnet(verb_longit)
#> ! Nodal columns with "NA" detected: "i_polity2". Ergm terms like
#>   `nodecov()`/`nodematch()` will refuse to fit.
#> ℹ Use `drop_na_actors(net, cols = c('i_polity2'))` (or impute) before
#>   refitting.
#> This message is displayed once per session.
length(sn_list)
#> [1] 3
names(sn_list)
#> [1] "2010" "2011" "2012"
class(sn_list[[1]])
#> [1] "network"
```

Fit a separate ergm per period:

``` r

set.seed(6886)
fits <- lapply(sn_list, function(n) {
    ergm(n ~ edges)
})
```

For *coevolution* models (where ties change as a function of past ties),
look at `tergm` from the statnet suite – `netify` provides the data,
`tergm` does the modeling. A typical tergm 4.x call against the same
per-period list looks like this:

``` r

# longitudinal ergm via tergm 4.x
library(tergm)
set.seed(6886)

nets <- to_statnet(verb_longit)   # named list of network objects
fit <- tergm(
    nets ~ Form(~ edges) +
           Persist(~ edges),
    estimate = "CMLE",
    times = seq_along(nets)
)
summary(fit)
```

In the tergm 4.x split formulation, `Form()` models the *formation* of
new edges between periods, and `Persist()` models the *persistence* of
existing edges from one period to the next; both formulas accept the
same ergm terms (`edges`, `ergm::nodecov()`, `ergm::nodematch()`,
`mutual`, `gwesp`, etc.). Add nodal covariates after applying the same
missing-data check shown in the cross-sectional example.

## 3. multilayer pipeline (per-layer fits)

Multilayer ergm modeling is its own research literature. The simple
pragmatic approach is to fit one ergm per layer.
[`to_statnet()`](https://netify-dev.github.io/netify/reference/netify_to_statnet.md)
handles this automatically: pass a multilayer netify, get back a named
list keyed by layer.

``` r

verb <- netify(icews_2010, actor1 = "i", actor2 = "j",
               symmetric = FALSE, weight = "verbCoop")
matl <- netify(icews_2010, actor1 = "i", actor2 = "j",
               symmetric = FALSE, weight = "matlCoop")
multi <- layer_netify(list(verbal = verb, material = matl))

sn_multi <- to_statnet(multi)
length(sn_multi)
#> [1] 2
names(sn_multi)
#> [1] "verbal"   "material"
```

Each element is a `network` object you can plug straight into
[`ergm()`](https://rdrr.io/pkg/ergm/man/ergm.html).

## 4. round-tripping simulated networks back to netify

A useful descriptive check after fitting an ergm is to simulate from the
fit, compute descriptives on the simulated networks, and compare to the
observed network. `simulate.ergm()` returns `network` objects, which
means you can pipe them straight back into a netify for any of netify’s
descriptive tools:

``` r

set.seed(6886)
# m is the fitted ergm object from the model chunk above
sims <- simulate(m, nsim = 100)  # list of network objects

# convert each simulated network back into a netify for comparison
sim_nets <- lapply(sims, function(s) to_netify(s))

# compare observed vs simulated at the structural level
all_nets <- c(list(observed = verb_coop), sim_nets)
struct_comp <- compare_networks(all_nets, what = "structure")
```

This gives observed-vs-simulated comparisons for density, reciprocity,
transitivity, mean degree, and related summaries.

## tl;dr

``` r

# build -> attach attrs -> export -> model
net <- netify(df, actor1 = "i", actor2 = "j", symmetric = FALSE, weight = "x")
net <- add_node_vars(net, attrs, actor = "id")
sn  <- to_statnet(net)               # single network, longit list, or multilayer list
m   <- ergm(sn ~ edges + ...)        # modeling happens in ergm/statnet
```

For latent-factor and DBN workflows, see the project-site article on
`lame` and `dbn`.

## references

1.  Butts, C. T. (2008). network: A Package for Managing Relational Data
    in R. Journal of Statistical Software, 24(2), 1-36.
    <doi:10.18637/jss.v024.i02>

2.  Cranmer, S. J., Desmarais, B. A., & Morgan, J. W. (2021).
    Inferential Network Analysis. Cambridge University Press.
    <doi:10.1017/9781316662915>

3.  Hunter, D. R., Handcock, M. S., Butts, C. T., Goodreau, S. M., &
    Morris, M. (2008). ergm: A Package to Fit, Simulate and Diagnose
    Exponential-Family Models for Networks. Journal of Statistical
    Software, 24(3), 1-29. <doi:10.18637/jss.v024.i03>

4.  Krivitsky, P. N., & Handcock, M. S. (2014). A Separable Model for
    Dynamic Networks. Journal of the Royal Statistical Society: Series B
    (Statistical Methodology), 76(1), 29-46. <doi:10.1111/rssb.12014>

5.  Snijders, T. A. B., Pattison, P. E., Robins, G. L., &
    Handcock, M. S. (2006). New Specifications for Exponential Random
    Graph Models. Sociological Methodology, 36(1), 99-153.
    <doi:10.1111/j.1467-9531.2006.00176.x>
