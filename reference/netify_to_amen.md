# Convert netify objects to amen format

`netify_to_amen` (also available as `to_amen`) transforms netify network
objects into the data structure required by the amen package for
advanced network modeling. this enables the use of social relations
models (srm), additive and multiplicative effects (ame) models, and
other network regression approaches implemented in amen.

## Usage

``` r
netify_to_amen(netlet, lame = FALSE)

to_amen(netlet, lame = FALSE)
```

## Arguments

- netlet:

  a netify object (class "netify") containing network data. must be a
  single-layer network. for multilayer networks, first extract
  individual layers using
  [`subset_netify`](https://netify-dev.github.io/netify/reference/subset_netify.md).

- lame:

  logical. controls the output format for longitudinal data:

  - `FALSE` (default): formats output for compatibility with the
    standard version of the amen package, which uses array structures. y
    is returned as a 3d array `[n_actors x n_actors x n_time]`, xdyad as
    a 4d array `[n_actors x n_actors x n_covariates x n_time]`, and
    xrow/xcol as 3d arrays `[n_actors x n_attributes x n_time]`. this
    requires constant actor composition across time.

  - `TRUE`: formats output for compatibility with the netify-verse
    version called lame, which supports longitudinal network modeling
    with time-varying actor compositions. y is returned as a list of t
    matrices (one per time period), xdyad as a list of t 3d arrays, and
    xrow/xcol as lists of t matrices. actor sets can vary across time
    periods, making this suitable for panels where countries enter/exit.

  this parameter is ignored for cross-sectional data.

## Value

the structure of the returned list depends on the data type and `lame`
parameter:

**for cross-sectional data or longitudinal with lame = FALSE (standard
amen):**

- **y**:

  network adjacency data as a numeric matrix or array:

  - cross-sectional: matrix of dimensions `[n_actors x n_actors]`

  - longitudinal: array of dimensions `[n_actors x n_actors x n_time]`

  contains edge weights or binary indicators. missing edges are
  preserved as na.

- **xdyad**:

  dyadic covariates as an array, or NULL if none exist:

  - cross-sectional: `[n_actors x n_actors x n_covariates]`

  - longitudinal: `[n_actors x n_actors x n_covariates x n_time]`

  each slice contains one dyadic covariate across all actor pairs.

- **xrow**:

  sender/row actor attributes as a matrix or array, or NULL if none
  exist:

  - cross-sectional: `[n_actors x n_attributes]`

  - longitudinal: `[n_actors x n_attributes x n_time]`

  contains numeric attributes for actors when they act as senders.

- **xcol**:

  receiver/column actor attributes, structured identically to xrow. for
  symmetric networks, xcol is identical to xrow. for bipartite networks,
  contains attributes for the second mode.

**for longitudinal data with lame = TRUE (lame package):**

- **y**:

  a list of length t (time periods), where each element is an n x n
  relational matrix. actor sets can vary across time periods.

- **xdyad**:

  a list of length t, where each element is an n x n x pd array of
  dyadic covariates, or NULL if none exist

- **xrow**:

  a list of length t, where each element is an n x pr matrix of nodal
  row covariates, or NULL if none exist

- **xcol**:

  a list of length t, where each element is an n x pc matrix of nodal
  column covariates, or NULL if none exist

## Details

**variable requirements:**

- all nodal attributes must be numeric (integer or double)

- all dyadic attributes must be numeric or logical matrices

- character or factor variables must be converted before using this
  function

- missing values (na) are preserved and can be handled by amen's models

**when to use each format:**

- use `lame = FALSE` when:

  - actor composition is constant across time

- use `lame = TRUE` when:

  - actors enter/exit the network over time

  - want access to other features in lame

## Note

the function performs several validation checks:

- ensures single-layer networks (multilayer not supported). for
  multilayer networks, first extract individual layers using
  [`subset_netify`](https://netify-dev.github.io/netify/reference/subset_netify.md)
  (e.g., `subset(net, layers = "trade")`).

- verifies all nodal and dyadic attributes are model-ready numeric
  inputs

- maintains actor ordering from the original netify object

for multilayer longitudinal models that require a 4d array
`[n, n, p, t]`, see
[`netify_to_dbn`](https://netify-dev.github.io/netify/reference/netify_to_dbn.md)
instead.

**bipartite networks.**
[`amen::ame()`](https://rdrr.io/pkg/amen/man/ame.html) does not accept
rectangular y matrices; passing the output of `to_amen()` on a bipartite
netify to [`amen::ame()`](https://rdrr.io/pkg/amen/man/ame.html) will
fail. use
[`netify_to_lame`](https://netify-dev.github.io/netify/reference/netify_to_lame.md)
(which sets `mode = "bipartite"` and targets `lame::ame()`) for
bipartite networks instead.

## Author

ha eun choi, cassy dorff, colin henry, shahryar minhas

cassy dorff, shahryar minhas

## Examples

``` r
# load example data
data(icews)

# create a netify object
net <- netify(
    icews[icews$year == 2010, ],
    actor1 = "i", actor2 = "j",
    symmetric = FALSE,
    weight = "verbCoop"
)

# convert to amen format (standard)
amen_data <- netify_to_amen(net)
names(amen_data) # y, xdyad, xrow, xcol
#> [1] "Y"     "Xdyad" "Xrow"  "Xcol" 

if (FALSE) { # \dontrun{
# for longitudinal data with time-varying composition
longit_net <- netify(
    icews,
    actor1 = "i", actor2 = "j", time = "year",
    symmetric = FALSE,
    weight = "verbCoop"
)

# convert to lame format
lame_data <- netify_to_amen(longit_net, lame = TRUE)
} # }
```
