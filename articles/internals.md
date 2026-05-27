# Internals: object structure & writing custom extensions

This vignette is for **package developers**, **methodologists writing
custom extensions**, and **anyone who wants to inspect the underlying
object structure**. End-user workflows are covered in
[`vignette("foundations_lite", package = "netify")`](https://netify-dev.github.io/netify/articles/foundations_lite.md)
and the topic-specific vignettes; this one goes one layer deeper.

``` r

library(netify)
data(icews)
```

## The netify object: a base R object with attributes

A **netify object** is a base R matrix / 3D array / list-of-matrices
with `class = "netify"` and a bundle of attributes carrying all
metadata. The `netify_type` attribute records one of three shapes:

| `netify_type` | Underlying R object | When used |
|----|----|----|
| `cross_sec` | `[n × n]` matrix (or `[r × c]` bipartite) | One time period |
| `longit_array` | `[n × n × T]` array (or `[n × n × p × T]`) | Multi-period, constant actor set |
| `longit_list` | Named list of matrices, one per time | Multi-period, varying actor composition |

Multilayer networks insert a layer dimension (position 3) and store
layer names in `attr(x, "layers")`. Mixed-directedness multilayer is
supported via a vector-valued `symmetric` attribute.

`dgCMatrix` inputs from the **Matrix** package are accepted but
densified at construction (a one-shot cli inform is emitted); a true
sparse storage backend is not yet implemented.

Inspect any netify object’s attribute bundle:

``` r

net <- netify(icews[icews$year == 2010, ],
              actor1 = "i", actor2 = "j",
              symmetric = FALSE, weight = "verbCoop",
              nodal_vars = "i_polity2",
              dyad_vars  = "matlCoop")
#> ℹ `missing_to_zero` is set to "TRUE" (the default).
#> ! Missing dyads will be filled with zeros. For latent space or other
#>   statistical network models, structural zeros and missing data have different
#>   meanings. Set `missing_to_zero = FALSE` to preserve NAs if this distinction
#>   matters for your analysis.
#> This message is displayed once per session.

class(net)
#> [1] "netify"
str(attributes(net), max.level = 1)
#> List of 17
#>  $ dim               : int [1:2] 152 152
#>  $ dimnames          :List of 2
#>  $ class             : chr "netify"
#>  $ netify_type       : chr "cross_sec"
#>  $ actor_time_uniform: logi TRUE
#>  $ actor_pds         :'data.frame':  152 obs. of  3 variables:
#>  $ weight            : chr "verbCoop"
#>  $ detail_weight     : chr "Weights from `verbCoop`"
#>  $ is_binary         : logi FALSE
#>  $ symmetric         : logi FALSE
#>  $ mode              : chr "unipartite"
#>  $ layers            : chr "verbCoop"
#>  $ diag_to_NA        : logi TRUE
#>  $ missing_to_zero   : logi TRUE
#>  $ sum_dyads         : logi FALSE
#>  $ nodal_data        :'data.frame':  152 obs. of  2 variables:
#>  $ dyad_data         :List of 1
```

Key attributes:

- `netify_type` — `"cross_sec"`, `"longit_array"`, or `"longit_list"`
- `mode` — `"unipartite"` or `"bipartite"`
- `symmetric` — scalar logical (or named vector for mixed-directedness
  multilayer)
- `weight` — column name (or `NULL` for binary)
- `is_binary`, `detail_weight`, `diag_to_NA`, `missing_to_zero`,
  `sum_dyads`
- `layers` — character vector; length \> 1 means multilayer
- `actor_pds` — data.frame with `actor`, `min_time`, `max_time` per
  actor
- `nodal_data` — data.frame with `actor`, optional `time`, and one
  column per nodal variable
- `dyad_data` — **nested list**: `list[[time]][[var]] = matrix`.
  Cross-sec uses `"1"` as the time key.

## Extracting parts

| Want this | Use |
|----|----|
| The raw matrix / array / list | `get_raw(net)` |
| A quick numeric peek | `peek(net, from = 5, to = 5)` |
| The full long edge data frame | `unnetify(net)` or `tidy(net)` |
| Lean wide-to-long edge frame (no nodal merge) | `melt(net)` |
| Graph-level stats | `summary(net)` or `glance(net)` |
| Actor-level stats | `summary_actor(net)` |
| Size / composition descriptors | `measurements(net)` |
| Edge data + layout for plotting | `net_plot_data(net)$net_dfs` |

[`tidy()`](https://generics.r-lib.org/reference/tidy.html) and
[`glance()`](https://generics.r-lib.org/reference/glance.html) are S3
methods on the broom generics — they’re registered on package load if
`generics` (or anything that imports it, like `broom`, `dplyr`,
`tidymodels`) is installed. No hard dependency on broom.

## Tidy interop

If `tibble` and `broom` are installed, three tidyverse-flavored entry
points are available. `as_tibble.netify` is registered against
[`tibble::as_tibble`](https://tibble.tidyverse.org/reference/as_tibble.html)
via `.onLoad`, so `tibble` must be installed to use the unprefixed call;
[`tidy()`](https://generics.r-lib.org/reference/tidy.html) and
[`glance()`](https://generics.r-lib.org/reference/glance.html) are
likewise registered against the broom generics.

``` r

library(tibble)
library(broom)

# one row per dyad (long edge frame, wrapped in a tibble)
as_tibble(net)
#> # A tibble: 22,952 × 6
#>    from        to         verbCoop matlCoop i_polity2_from i_polity2_to
#>    <chr>       <chr>         <dbl>    <dbl>          <int>        <int>
#>  1 Afghanistan Albania           0        1             NA            9
#>  2 Afghanistan Algeria           0        0             NA            2
#>  3 Afghanistan Angola            0        0             NA           -2
#>  4 Afghanistan Argentina         1        0             NA            8
#>  5 Afghanistan Armenia           7        2             NA            5
#>  6 Afghanistan Australia       125        0             NA           10
#>  7 Afghanistan Austria           1        0             NA           10
#>  8 Afghanistan Azerbaijan        7        0             NA           -7
#>  9 Afghanistan Bahrain           3        0             NA           -5
#> 10 Afghanistan Bangladesh       14        0             NA            5
#> # ℹ 22,942 more rows

# broom-style tidy summary: a tibble with one row per (non-zero) dyad
head(tidy(net))
#> # A tibble: 6 × 6
#>   from        to         verbCoop matlCoop i_polity2_from i_polity2_to
#>   <chr>       <chr>         <dbl>    <dbl>          <int>        <int>
#> 1 Afghanistan Argentina         1        0             NA            8
#> 2 Afghanistan Armenia           7        2             NA            5
#> 3 Afghanistan Australia       125        0             NA           10
#> 4 Afghanistan Austria           1        0             NA           10
#> 5 Afghanistan Azerbaijan        7        0             NA           -7
#> 6 Afghanistan Bahrain           3        0             NA           -5

# one-row-per-network model-card summary (one row per time/layer if applicable)
glance(net)
#> # A tibble: 1 × 18
#>   net   num_actors density num_edges prop_edges_missing mean_edge_weight
#>   <chr>      <dbl>   <dbl>     <dbl>              <dbl>            <dbl>
#> 1 1            152   0.435      9976                  0             18.1
#> # ℹ 12 more variables: sd_edge_weight <dbl>, median_edge_weight <dbl>,
#> #   min_edge_weight <dbl>, max_edge_weight <dbl>, competition_row <dbl>,
#> #   competition_col <dbl>, sd_of_row_means <dbl>, sd_of_col_means <dbl>,
#> #   covar_of_row_col_means <dbl>, reciprocity <dbl>, mutual <dbl>,
#> #   transitivity <dbl>
```

`as_tibble(net)` and `tidy(net)` share the same long-format payload as
`unnetify(net)` — they differ only in whether zero-weight edges are
dropped by default
([`tidy()`](https://generics.r-lib.org/reference/tidy.html) drops them,
matching the broom convention). `glance(net)` is the broom-flavored
sibling of `summary(net)` — one row of graph-level statistics per
network (or per (time, layer) slice for longitudinal / multilayer
inputs).

[`as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
also has a method for `netify_comparison` objects (from
[`compare_networks()`](https://netify-dev.github.io/netify/reference/compare_networks.md)),
returning the per-pair `$comparisons` frame directly so you can pipe
straight into
[`filter()`](https://dplyr.tidyverse.org/reference/filter.html) /
[`arrange()`](https://dplyr.tidyverse.org/reference/arrange.html) /
`pivot_wider()`:

``` r

cmp <- compare_networks(list("2010" = net1, "2011" = net2))
as_tibble(cmp)
#> # A tibble: 1 x 5
#>   net_i net_j metric      value p_value
#>   <chr> <chr> <chr>       <dbl>   <dbl>
#> 1 2011  2010  correlation 0.884      NA
```

## Predicates and descriptors

For programmatic dispatch — e.g. inside a custom exporter or pipeline
step — netify ships a small set of non-masking predicates and size
accessors:

``` r

# class / structure predicates
is_netify(net)
#> [1] TRUE
is_bipartite(net)          # may be masked by igraph::is_bipartite
#> [1] FALSE
is_bipartite_netify(net)   # alias that won't be masked
#> [1] FALSE
is_directed_netify(net)
#> [1] TRUE
is_longitudinal(net)
#> [1] FALSE
is_multilayer(net)
#> [1] FALSE

# size / composition accessors
n_actors(net)              # number of unique actors
#> [1] 152
n_periods(net)             # number of time periods (1 for cross-sec)
#> [1] 1
n_layers(net)              # number of layers (1 for single-layer)
#> [1] 1
get_actor_time_info(net)   # returns the stored actor_pds: actor, min_time, max_time
#>                                          actor min_time max_time
#> 1                                  Afghanistan        1        1
#> 2                                      Albania        1        1
#> 3                                      Algeria        1        1
#> 4                                       Angola        1        1
#> 5                                    Argentina        1        1
#> 6                                      Armenia        1        1
#> 7                                    Australia        1        1
#> 8                                      Austria        1        1
#> 9                                   Azerbaijan        1        1
#> 10                                     Bahrain        1        1
#> 11                                  Bangladesh        1        1
#> 12                                     Belarus        1        1
#> 13                                     Belgium        1        1
#> 14                                       Benin        1        1
#> 15             Bolivia, Plurinational State Of        1        1
#> 16                      Bosnia And Herzegovina        1        1
#> 17                                      Brazil        1        1
#> 18                                    Bulgaria        1        1
#> 19                                Burkina Faso        1        1
#> 20                                     Burundi        1        1
#> 21                                    Cambodia        1        1
#> 22                                    Cameroon        1        1
#> 23                                      Canada        1        1
#> 24                                  Cape Verde        1        1
#> 25                    Central African Republic        1        1
#> 26                                        Chad        1        1
#> 27                                       Chile        1        1
#> 28                                       China        1        1
#> 29                                    Colombia        1        1
#> 30                                     Comoros        1        1
#> 31                          Congo, Republic Of        1        1
#> 32           Congo, The Democratic Republic Of        1        1
#> 33                                  Costa Rica        1        1
#> 34                               Cote D'ivoire        1        1
#> 35                                     Croatia        1        1
#> 36                                        Cuba        1        1
#> 37                                      Cyprus        1        1
#> 38                                     Denmark        1        1
#> 39                                    Djibouti        1        1
#> 40                          Dominican Republic        1        1
#> 41                                     Ecuador        1        1
#> 42                                       Egypt        1        1
#> 43                                 El Salvador        1        1
#> 44                           Equatorial Guinea        1        1
#> 45                                     Estonia        1        1
#> 46                                    Ethiopia        1        1
#> 47                                        Fiji        1        1
#> 48                                     Finland        1        1
#> 49                                      France        1        1
#> 50                                       Gabon        1        1
#> 51                                      Gambia        1        1
#> 52                                     Georgia        1        1
#> 53                                     Germany        1        1
#> 54                                       Ghana        1        1
#> 55                                      Greece        1        1
#> 56                                   Guatemala        1        1
#> 57                                      Guinea        1        1
#> 58                               Guinea-Bissau        1        1
#> 59                                      Guyana        1        1
#> 60                                       Haiti        1        1
#> 61                                    Honduras        1        1
#> 62                                     Hungary        1        1
#> 63                                       India        1        1
#> 64                                   Indonesia        1        1
#> 65                   Iran, Islamic Republic Of        1        1
#> 66                                        Iraq        1        1
#> 67                                     Ireland        1        1
#> 68                                      Israel        1        1
#> 69                                       Italy        1        1
#> 70                                     Jamaica        1        1
#> 71                                       Japan        1        1
#> 72                                      Jordan        1        1
#> 73                                  Kazakhstan        1        1
#> 74                                       Kenya        1        1
#> 75      Korea, Democratic People's Republic Of        1        1
#> 76                          Korea, Republic Of        1        1
#> 77                                      Kuwait        1        1
#> 78                                  Kyrgyzstan        1        1
#> 79            Lao People's Democratic Republic        1        1
#> 80                                      Latvia        1        1
#> 81                                     Lebanon        1        1
#> 82                                     Liberia        1        1
#> 83                      Libyan Arab Jamahiriya        1        1
#> 84                                   Lithuania        1        1
#> 85                                  Luxembourg        1        1
#> 86  Macedonia, The Former Yugoslav Republic Of        1        1
#> 87                                  Madagascar        1        1
#> 88                                      Malawi        1        1
#> 89                                    Malaysia        1        1
#> 90                                        Mali        1        1
#> 91                                  Mauritania        1        1
#> 92                                   Mauritius        1        1
#> 93                                      Mexico        1        1
#> 94                        Moldova, Republic Of        1        1
#> 95                                    Mongolia        1        1
#> 96                                     Morocco        1        1
#> 97                                  Mozambique        1        1
#> 98                                     Myanmar        1        1
#> 99                                       Nepal        1        1
#> 100                                Netherlands        1        1
#> 101                                New Zealand        1        1
#> 102                                  Nicaragua        1        1
#> 103                                      Niger        1        1
#> 104                                    Nigeria        1        1
#> 105                                     Norway        1        1
#> 106                                       Oman        1        1
#> 107                                   Pakistan        1        1
#> 108                                     Panama        1        1
#> 109                           Papua New Guinea        1        1
#> 110                                   Paraguay        1        1
#> 111                                       Peru        1        1
#> 112                                Philippines        1        1
#> 113                                     Poland        1        1
#> 114                                   Portugal        1        1
#> 115                                      Qatar        1        1
#> 116                                    Romania        1        1
#> 117                         Russian Federation        1        1
#> 118                                     Rwanda        1        1
#> 119                               Saudi Arabia        1        1
#> 120                                    Senegal        1        1
#> 121                               Sierra Leone        1        1
#> 122                                  Singapore        1        1
#> 123                                   Slovakia        1        1
#> 124                                   Slovenia        1        1
#> 125                            Solomon Islands        1        1
#> 126                                    Somalia        1        1
#> 127                               South Africa        1        1
#> 128                                      Spain        1        1
#> 129                                  Sri Lanka        1        1
#> 130                                      Sudan        1        1
#> 131                                   Suriname        1        1
#> 132                                     Sweden        1        1
#> 133                                Switzerland        1        1
#> 134                       Syrian Arab Republic        1        1
#> 135                                 Tajikistan        1        1
#> 136               Tanzania, United Republic Of        1        1
#> 137                                   Thailand        1        1
#> 138                                       Togo        1        1
#> 139                        Trinidad And Tobago        1        1
#> 140                                    Tunisia        1        1
#> 141                               Turkmenistan        1        1
#> 142                                     Uganda        1        1
#> 143                                    Ukraine        1        1
#> 144                       United Arab Emirates        1        1
#> 145                             United Kingdom        1        1
#> 146                              United States        1        1
#> 147                                    Uruguay        1        1
#> 148                                 Uzbekistan        1        1
#> 149                                    Vietnam        1        1
#> 150                                      Yemen        1        1
#> 151                                     Zambia        1        1
#> 152                                   Zimbabwe        1        1
```

The `_netify` suffix on
[`is_bipartite_netify()`](https://netify-dev.github.io/netify/reference/netify_predicates.md)
and
[`is_directed_netify()`](https://netify-dev.github.io/netify/reference/netify_predicates.md)
avoids masking the same-named predicates from `igraph` and `network`.
The unsuffixed versions exist too and defer to the foreign package if a
non-netify graph object is passed.

## Object-level validation

[`validate_netify()`](https://netify-dev.github.io/netify/reference/validate_netify.md)
is the developer’s pre-flight check. It walks the attribute bundle and
confirms the invariants the rest of the package relies on:

``` r

validate_netify(net, verbose = TRUE)
#> ✔ netify object passes all coherence checks (11 checks).
```

The full list of checks:

- `netify_type` — one of `"cross_sec"`, `"longit_array"`,
  `"longit_list"`, and matches the underlying R object
- `mode` — `"unipartite"` or `"bipartite"`
- `symmetric_type` — scalar logical, or named logical of length
  `n_layers` for mixed-directedness multilayer
- `layers_consistent` — `attr(net, "layers")` length matches the layer
  dimension where applicable
- `nodal_actors_known` — every actor referenced in `nodal_data` exists
  in the network’s actor set
- `is_binary_consistent` — the stored `is_binary` flag matches the
  actual content
- `symmetric_consistent` — if stored as symmetric, the matrix content is
  actually symmetric
- `unipartite_dimnames` — row and column names agree for unipartite
  networks
- `slice_dimnames_consistent` — every slice of a longitudinal array/list
  has dimnames in the order recorded in `actor_pds`
- `nodal_time_known` — time keys in nodal/dyad data are a subset of the
  network’s time axis

A quick demo on a clean netlet versus one tampered to introduce a stray
actor:

``` r

# clean netlet ticks every box
all(unlist(validate_netify(net, verbose = FALSE)))
#> [1] TRUE

# tamper: inject a stray actor into nodal_data
bad <- net
nd <- attr(bad, "nodal_data")
nd <- rbind(nd, nd[1, , drop = FALSE])
nd$actor[nrow(nd)] <- "ZZZ_not_in_network"
attr(bad, "nodal_data") <- nd

validate_netify(bad, verbose = TRUE)
#> ! nodal_data references 1 actor not in the netlet: "ZZZ_not_in_network"
#> ✖ netify object failed 1 coherence check: "nodal_actors_known".
```

If a custom exporter or an internal manipulation breaks one of these,
[`validate_netify()`](https://netify-dev.github.io/netify/reference/validate_netify.md)
is the first place to look.

## Open-cohort panels with `actor_pds`

When the actor roster changes over time — entries, exits, attrition,
contact-tracing windows — pass `actor_time_uniform = FALSE` and supply
an `actor_pds` data.frame giving each actor’s `[min_time, max_time]`
window. Each period’s netlet then contains only the actors whose window
covers that period; densities, degree counts, and per-period actor sets
respect those entry / exit boundaries.

### The `actor_pds` roster, step by step

Three pieces have to line up for an open-cohort netlet to be
well-formed:

1.  **A roster** — one row per actor, with `actor`, `min_time`,
    `max_time`. The `[min_time, max_time]` interval is *closed* (the
    actor is alive in the boundary periods themselves).
2.  **An edgelist** — one row per observed interaction. Edges whose
    endpoints or time fall outside the roster’s windows are silently
    dropped during construction.
3.  **`netify(actor_time_uniform = FALSE, actor_pds = roster, ...)`** —
    this is what tells the constructor to honor the roster instead of
    treating every actor as present in every period.

Below, actor `a` is in the network only during periods 1-2 (enters at
`t = 1`, exits after `t = 2`), and actors `d` / `e` arrive at `t = 3`.
The edgelist deliberately includes a tie at `t = 2` involving `a` and
ties at `t = 3` involving `d` so we can verify the period-by-period
actor sets afterwards.

``` r

set.seed(1)

# roster: actors with closed-interval entry / exit times
roster <- data.frame(
    actor = c("a", "b", "c", "d", "e"),
    min_time = c(1, 1, 1, 3, 3),
    max_time = c(2, 5, 4, 5, 5)   # a exits after t = 2
)

# edges (only show up while both endpoints are in the roster)
edges <- data.frame(
    i = c("a", "a", "b", "c", "d", "c", "d", "e"),
    j = c("b", "c", "c", "b", "e", "d", "e", "b"),
    t = c(1, 2, 2, 3, 4, 3, 5, 5)
)

net_oc <- netify(edges,
    actor1 = "i", actor2 = "j", time = "t",
    actor_time_uniform = FALSE,
    actor_pds = roster
)

# read the roster back off the netlet itself
get_actor_time_info(net_oc)
#>   actor min_time max_time
#> 1     a        1        2
#> 2     b        1        5
#> 3     c        1        4
#> 4     d        3        5
#> 5     e        3        5
n_actors(net_oc)
#> [1] 4
n_periods(net_oc)
#> [1] 5
```

[`get_actor_time_info()`](https://netify-dev.github.io/netify/reference/get_actor_time_info.md)
on a netify object returns the stored `actor_pds` directly (it is also
the argument name on
[`netify()`](https://netify-dev.github.io/netify/reference/netify.md)
itself, so the round trip is exact). On a raw dyad data.frame the same
function *derives* the roster from observed activity — that’s how you
build the `actor_pds` argument in the first place.

This is the standard path for open-cohort longitudinal data — panel
surveys with attrition, contact-tracing chains, organizational
membership over time, animal co-occurrence with births / deaths, and
similar settings where treating every actor as present in every period
would distort denominators.

### Density and per-period actor sets

The key correctness guarantee: density (and every other per-period
statistic) is computed against the actor set *alive in that period*, not
the union of all actors ever observed. With the roster above:

- `t = 1`: actors `a`, `b`, `c` are alive (3 actors)
- `t = 2`: actors `a`, `b`, `c` are alive (3 actors; `a`’s last period)
- `t = 3`: actors `b`, `c`, `d`, `e` are alive (4 actors; `a` is now
  out, `d` / `e` enter)
- `t = 4`: actors `b`, `c`, `d`, `e` are alive (4 actors)
- `t = 5`: actors `b`, `d`, `e` are alive (3 actors; `c`’s last period
  was 4)

``` r

oc_summary <- summary(net_oc)
oc_summary[, c("net", "num_actors", "density", "num_edges")]
#>   net num_actors   density num_edges
#> 1   1          3 0.3333333         1
#> 2   2          3 0.6666667         2
#> 3   3          4 0.3333333         2
#> 4   4          4 0.1666667         1
#> 5   5          3 0.6666667         2
```

The period-3 denominator is **4 × 3 = 12** (directed) or **4 × 3 / 2 =
6** (symmetric), *not* 5 × 4 — actor `a` is not counted because its
`max_time` is 2. This is the load-bearing invariant: density at `t = 3`
excludes the actor present only in periods 1-2, so a researcher
comparing densities across periods is not comparing apples to oranges.

The same accounting flows into
[`summary_actor()`](https://netify-dev.github.io/netify/reference/summary_actor.md),
[`homophily()`](https://netify-dev.github.io/netify/reference/homophily.md),
[`mixing_matrix()`](https://netify-dev.github.io/netify/reference/mixing_matrix.md),
and the plot helpers — open-cohort actors never show up as zero-degree
placeholders in periods they did not exist.

## NA versus zero in weighted networks

For weighted data, the distinction between `0` (observed, but no edge /
zero-valued interaction) and `NA` (unobserved / not-at-risk) is
semantically important in many domains — epidemiology, animal behavior,
sparse survey rosters. By default `netify(missing_to_zero = TRUE)` fills
unobserved dyads with `0`. Pass `missing_to_zero = FALSE` to keep them
as `NA`, in which case `summary()$prop_edges_missing` reports the
non-trivial missingness fraction and downstream centrality / homophily
routines propagate the NA semantics rather than silently treating the
dyad as a zero-weight tie.

Both `prop_edges_missing` and `prop_unknown_edges` use the same
denominator as `density` — the number of *potential edge dyads*
(off-diagonal for unipartite + `diag_to_NA`, halved for symmetric, all
cells for bipartite). That means
`density + prop_edges_missing + observed_zero_fraction = 1` is an
identity, which is the property you want when you are reading them as
competing fractions. The `prop_unknown_edges` column is suppressed when
`missing_to_zero = TRUE` because every unobserved dyad has been filled
with 0 and the value would be identically zero in every row; when
present it tracks `prop_edges_missing` and serves as the “this netlet
carries NA semantics” cue downstream.

## Writing a custom graph-level statistic

`summary(net, other_stats = list(my_stat = fn))` accepts user-supplied
functions. Each function receives the **netify object** for the current
time period / layer (not a stripped matrix), so you can call
[`netify_to_igraph()`](https://netify-dev.github.io/netify/reference/netify_to_igraph.md),
[`peek()`](https://netify-dev.github.io/netify/reference/peek.md), or
whatever you want inside.

Return a **named numeric vector** — names become column names in the
output frame.

``` r

# Example: number of weakly connected components with at least 2 nodes
n_components_2plus <- function(net) {
  g <- netify_to_igraph(net)
  c(n_components_2plus = sum(igraph::components(g)$csize >= 2))
}

# Example: edge weight skewness
weight_skew <- function(net) {
  v <- as.vector(net)
  v <- v[!is.na(v) & v != 0]
  if (length(v) < 3) return(c(weight_skew = NA_real_))
  c(weight_skew = mean((v - mean(v))^3) / (stats::sd(v)^3))
}

summary(net, other_stats = list(
  comp = n_components_2plus,
  skew = weight_skew
))
#>   net num_actors   density num_edges prop_edges_missing mean_edge_weight
#> 1   1        152 0.4346462      9976                  0         18.13393
#>   sd_edge_weight median_edge_weight min_edge_weight max_edge_weight
#> 1        132.367                  0               0            4937
#>   competition_row competition_col sd_of_row_means sd_of_col_means
#> 1      0.04071163      0.03493156         41.4412        37.76971
#>   covar_of_row_col_means reciprocity    mutual transitivity
#> 1              0.9934913   0.9823385 0.8402509    0.6458631
#>   comp.n_components_2plus skew.weight_skew
#> 1                       1         13.76129
```

The same `other_stats` mechanism is available in
[`summary_actor()`](https://netify-dev.github.io/netify/reference/summary_actor.md),
[`compare_networks()`](https://netify-dev.github.io/netify/reference/compare_networks.md),
[`homophily()`](https://netify-dev.github.io/netify/reference/homophily.md),
[`mixing_matrix()`](https://netify-dev.github.io/netify/reference/mixing_matrix.md),
and
[`dyad_correlation()`](https://netify-dev.github.io/netify/reference/dyad_correlation.md).
In each case the function gets called once per time period / layer /
iteration as appropriate; check the function’s `?` for the exact
contract.

## Writing a custom actor-level statistic

`summary_actor(net, other_stats = list(my_stat = fn))`. The function
should return a vector with one value per actor (in row order):

``` r

# Example: per-actor mean tie weight to non-isolates
mean_active_tie <- function(mat) {
  apply(mat, 1, function(row) {
    nonzero <- row[!is.na(row) & row != 0]
    if (length(nonzero) == 0) NA_real_ else mean(nonzero)
  })
}

head(summary_actor(net, other_stats = list(mean_active = mean_active_tie)))
#> Warning in betweenness_cutoff_impl(graph = graph, vids = v, directed = directed, : Some weights are smaller than epsilon, calculations may suffer from numerical precision issues.
#> Source: centrality/betweenness.c:441
#>         actor degree_in degree_out degree_total prop_ties_in prop_ties_out
#> 1 Afghanistan        95         80          175    0.6250000     0.5263158
#> 2     Albania        53         51          104    0.3486842     0.3355263
#> 3     Algeria        78         94          172    0.5131579     0.6184211
#> 4      Angola        68         61          129    0.4473684     0.4013158
#> 5   Argentina        64         66          130    0.4210526     0.4342105
#> 6     Armenia        56         56          112    0.3684211     0.3684211
#>   prop_ties_total network_share_in network_share_out network_share_total
#> 1               1      0.025958050       0.021073497         0.023515773
#> 2               1      0.001170082       0.001158069         0.001164076
#> 3               1      0.002919199       0.003056150         0.002987674
#> 4               1      0.002349775       0.002011004         0.002180390
#> 5               1      0.003925903       0.004175777         0.004050840
#> 6               1      0.004995075       0.004862930         0.004929002
#>   closeness_in closeness_out closeness_all betweenness authority_score
#> 1    128.82255     112.49032      202.3025 0.119495953     0.470595036
#> 2    128.88784     111.88795      202.3025 0.084083885     0.006988585
#> 3    110.00485     108.85637      202.3025 0.021258278     0.022841447
#> 4     62.87728      86.84856      202.3025 0.012737307     0.016299497
#> 5     81.34646     109.64682      202.3025 0.008222958     0.033471704
#> 6    107.69934      94.19641      152.5994 0.000000000     0.064875541
#>     hub_score strength_sum_in strength_sum_out strength_sum_total
#> 1 0.233837051           10804             8771              19575
#> 2 0.005265195             487              482                969
#> 3 0.014840880            1215             1272               2487
#> 4 0.009186279             978              837               1815
#> 5 0.023840418            1634             1738               3372
#> 6 0.039673958            2079             2024               4103
#>   strength_avg_in strength_avg_out strength_avg_total strength_std_in
#> 1       71.549669        58.086093          64.817881      350.116660
#> 2        3.225166         3.192053           3.208609        7.694303
#> 3        8.046358         8.423841           8.235099       22.421221
#> 4        6.476821         5.543046           6.009934       16.701231
#> 5       10.821192        11.509934          11.165563       31.414452
#> 6       13.768212        13.403974          13.586093       65.122392
#>   strength_std_out strength_std_total strength_median_in strength_median_out
#> 1        285.28528         317.700972                  2                   1
#> 2          8.35122           8.022761                  0                   0
#> 3         21.96283          22.192025                  1                   1
#> 4         14.70543          15.703333                  0                   0
#> 5         32.11954          31.766997                  0                   0
#> 6         63.81538          64.468885                  0                   0
#>   strength_median_total mean_active
#> 1                   1.5   109.63750
#> 2                   0.0     9.45098
#> 3                   1.0    13.53191
#> 4                   0.0    13.72131
#> 5                   0.0    26.33333
#> 6                   0.0    36.14286
```

## Reading the dyad_data nested list

`attr(net, "dyad_data")` is the structure that gives netify O(1) access
to per-time dyadic covariates. Its shape is:

``` r

dd <- attr(net, "dyad_data")
names(dd)             # cross-sec: just "1"
#> [1] "1"
names(dd[["1"]])      # one entry per dyadic variable
#> [1] "matlCoop"
str(dd[["1"]][["matlCoop"]])
#>  int [1:152, 1:152] 0 4 0 0 0 2 19 0 1 0 ...
#>  - attr(*, "dimnames")=List of 2
#>   ..$ : chr [1:152] "Afghanistan" "Albania" "Algeria" "Angola" ...
#>   ..$ : chr [1:152] "Afghanistan" "Albania" "Algeria" "Angola" ...
```

For longitudinal networks the top level has one entry per period:

``` r

# pseudo-structure for a 3-period network with 2 dyadic vars:
# dd[["2010"]][["matlCoop"]]   -> n×n matrix
# dd[["2010"]][["verbConf"]]   -> n×n matrix
# dd[["2011"]][["matlCoop"]]   -> n×n matrix
# ... etc.
```

If you’re writing a converter (e.g., to a new modeling format), this is
the structure to iterate over.

## Writing a custom exporter (`to_*` function)

The existing
[`to_amen()`](https://netify-dev.github.io/netify/reference/netify_to_amen.md),
[`to_lame()`](https://netify-dev.github.io/netify/reference/netify_to_lame.md),
[`to_dbn()`](https://netify-dev.github.io/netify/reference/netify_to_dbn.md),
[`to_igraph()`](https://netify-dev.github.io/netify/reference/netify_to_igraph.md),
[`to_statnet()`](https://netify-dev.github.io/netify/reference/netify_to_statnet.md)
give you templates.
([`to_lame()`](https://netify-dev.github.io/netify/reference/netify_to_lame.md)
is the modern AME / latent-factor handoff and emits a snippet of
executable code under the binding `nl`;
[`to_amen()`](https://netify-dev.github.io/netify/reference/netify_to_amen.md)
is the historical alias and now redirects to
[`to_lame()`](https://netify-dev.github.io/netify/reference/netify_to_lame.md).)
A new exporter generally needs to:

1.  Validate the netify object with `netify_check(netlet)`
2.  Branch on `attr(netlet, "netify_type")` (cross_sec / longit_array /
    longit_list)
3.  For multilayer, decide whether to iterate per layer (use
    `subset_netify(netlet, layers = X)` and return a named list) or to
    produce a joint structure (4D array, etc.)
4.  Pull the raw network data with `get_raw(netlet)`
5.  Pull nodal data with `attr(netlet, "nodal_data")` and align to your
    target package’s actor order
6.  Pull dyadic data with `attr(netlet, "dyad_data")` and reshape to
    your target format
7.  Handle missing values according to your target package’s convention
    (most don’t accept NAs)

If your target package expects per-layer outputs but takes multilayer
netify inputs, the standard pattern is:

``` r

to_mymodel <- function(netlet, ...) {
    netify_check(netlet)
    layer_names <- attributes(netlet)$layers
    if (length(layer_names) > 1) {
        out <- lapply(layer_names, function(lyr) {
            to_mymodel(subset_netify(netlet, layers = lyr), ...)
        })
        names(out) <- layer_names
        return(out)
    }
    # ... single-layer logic ...
}
```

This is what
[`to_igraph()`](https://netify-dev.github.io/netify/reference/netify_to_igraph.md),
[`to_statnet()`](https://netify-dev.github.io/netify/reference/netify_to_statnet.md),
and
[`to_lame()`](https://netify-dev.github.io/netify/reference/netify_to_lame.md)
do.

## Performance characteristics

| Operation | Complexity | Notes |
|----|----|----|
| `netify(df)` cross-sec | O(N × E) | C++ via Rcpp; fast |
| `netify(df, time = ...)` longit | O(N × E × T) | C++; fast |
| `summary(net)` | O(N²) per period | igraph backend |
| `summary_actor(net)` | O(N²) per period × per layer | igraph; closeness/betweenness dominate |
| `summary_actor(net, bootstrap = TRUE)` | O(B × n × N²) | linear in `n_boot`; defaults to 200 |
| `compare_networks(method = "qap")` | O(R × N²) | C++ permutations; tunable via `n_permutations` |
| `compare_networks(method = "spectral")` | O(N³) | use `spectral_rank = round(sqrt(N))` for large nets |
| `unnetify(net)` | O(N² × T) | use `remove_zeros = TRUE` for sparse output |
| `melt(net)` | O(N² × T) | leaner than `unnetify`; no nodal merge |
| `plot(net)` | O(N²) layout + O(E) render | igraph layout dominates |

For networks of a few hundred actors over a dozen time periods,
everything runs in seconds. The C++ routines for
[`netify()`](https://netify-dev.github.io/netify/reference/netify.md),
[`compare_networks()`](https://netify-dev.github.io/netify/reference/compare_networks.md)
QAP, and similarity calculations handle the heaviest paths. The R-side
wrappers (especially in plotting and attribute handling) are not as
optimized.

**Memory budget rule of thumb.** netify stores adjacencies densely: an N
× N double-precision matrix is 8·N² bytes. For longitudinal stacks the
cost is 8·N²·T (`longit_array`) or the sum over per-period sizes
(`longit_list`, when actor composition varies). Concretely:

| N      | per-snapshot RAM | T = 12 (monthly year) | T = 52 (weekly year) |
|--------|------------------|-----------------------|----------------------|
| 200    | 320 KB           | 3.8 MB                | 17 MB                |
| 1,000  | 8 MB             | 96 MB                 | 416 MB               |
| 5,000  | 200 MB           | 2.4 GB                | 10 GB                |
| 10,000 | 800 MB           | 9.4 GB                | 41 GB                |
| 50,000 | 20 GB            | 234 GB                | 1 TB                 |

`longit_list` netlets cost the *sum* of per-period sizes rather than
`T × max(N)²`, so they pay less when actor composition is sparse over
time. For 15,000-node weekly Twitter snapshots over a year (`T = 52`), a
`longit_array` is ~93 GB and would not fit on a laptop; a `longit_list`
whose typical-period N is much smaller is the only viable in-memory
shape.

Above ~10,000 actors, prefer building the netlet from an edgelist
data.frame (skips the dense intermediate at construction) and consider
exiting to an edge data.frame via `unnetify(net, remove_zeros = TRUE)`,
or to `igraph` via `to_igraph(net)` for community detection and other
large-N algorithms. `summary_actor(stats = "fast")` skips closeness /
betweenness / eigen / HITS and auto-promotes when N exceeds
`getOption("netify.fast_threshold", 1500L)`.

**Sparse-input guard.** Passing a `Matrix::dgCMatrix` (or any
`sparseMatrix`) with density \< 1% and N \> 5,000 aborts construction
with a pointer to the edgelist path. The motivating case is a 15K × 15K
follower graph at density 0.001: densifying allocates ~1.7 GB of mostly
zeros, which is almost never what the caller wants. Pass
`force_dense = TRUE` to override if you really do want the dense
allocation.

**Benchmark, three ER networks.** Wall-clock for the dense-matrix path
on a representative laptop (single core, single snapshot, p = 0.01).
Re-run locally with the chunk below if you want numbers for your own
machine.

``` r

library(netify)
set.seed(1)
bench_one <- function(N, p = 0.01) {
  # build an ER adjacency directly as an edgelist (skips the dense intermediate)
  i <- sample.int(N, size = round(p * N * N), replace = TRUE)
  j <- sample.int(N, size = length(i), replace = TRUE)
  df <- data.frame(from = i, to = j)

  t0 <- Sys.time(); net <- netify(df, actor1 = "from", actor2 = "to"); t_build <- Sys.time() - t0
  t0 <- Sys.time(); s   <- summary(net);                                 t_summary <- Sys.time() - t0
  t0 <- Sys.time(); sa  <- summary_actor(net, stats = "fast");           t_actor_fast <- Sys.time() - t0
  t0 <- Sys.time(); ig  <- to_igraph(net);                               t_igraph <- Sys.time() - t0

  data.frame(N = N,
             build_s = as.numeric(t_build, units = "secs"),
             summary_s = as.numeric(t_summary, units = "secs"),
             summary_actor_fast_s = as.numeric(t_actor_fast, units = "secs"),
             to_igraph_s = as.numeric(t_igraph, units = "secs"))
}
do.call(rbind, lapply(c(1000, 5000, 10000), bench_one))
```

Indicative results (single-core, 16 GB laptop):

| N | [`netify()`](https://netify-dev.github.io/netify/reference/netify.md) | [`summary()`](https://rdrr.io/r/base/summary.html) | `summary_actor(stats = "fast")` | [`to_igraph()`](https://netify-dev.github.io/netify/reference/netify_to_igraph.md) |
|----|----|----|----|----|
| 1,000 | \< 1 s | \< 1 s | \< 1 s | \< 1 s |
| 5,000 | ~2 s | ~4 s | ~1 s | \< 1 s |
| 10,000 | ~2 s | ~14 s | ~3 s | ~3 s |

[`summary()`](https://rdrr.io/r/base/summary.html) is dominated by the
igraph-based global metrics and grows fastest;
[`netify()`](https://netify-dev.github.io/netify/reference/netify.md)
itself stays under a few seconds even at N = 10,000 when fed an
edgelist. The full
[`summary_actor()`](https://netify-dev.github.io/netify/reference/summary_actor.md)
(with closeness / betweenness / eigen / HITS) blows up roughly
quadratically — at N = 10,000 it takes several minutes versus the few
seconds shown above for the fast path. That’s why the auto-promote fires
by default.

## See also

- [`vignette("foundations_lite", package = "netify")`](https://netify-dev.github.io/netify/articles/foundations_lite.md)
  — minimal end-to-end tour
- [`vignette("foundations", package = "netify")`](https://netify-dev.github.io/netify/articles/foundations.md)
  — full IR walkthrough
- [`vignette("pipeline_lame_dbn", package = "netify")`](https://netify-dev.github.io/netify/articles/pipeline_lame_dbn.md)
  — modeling handoff to amen / lame / dbn
- [`vignette("pipeline_netify_ergm", package = "netify")`](https://netify-dev.github.io/netify/articles/pipeline_netify_ergm.md)
  — modeling handoff to ergm / statnet
