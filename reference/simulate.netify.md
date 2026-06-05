# Simulate NULL-model networks from a netify object

generates `nsim` new netify objects from one of three standard NULL
models, holding the actor set fixed (and, where applicable, the observed
density / degree sequence). useful for sanity-checking whether an
observed network statistic (transitivity, modularity, etc.) is
surprising relative to a chance benchmark, without reaching for
`statnet::ergm` for a simple NULL.

## Usage

``` r
# S3 method for class 'netify'
simulate(
  object,
  nsim = 1L,
  seed = NULL,
  model = c("erdos_renyi", "configuration", "dyad_permutation"),
  ...
)
```

## Arguments

- object:

  a netify object (cross-sectional or per-period; for longitudinal input
  each period is simulated independently).

- nsim:

  integer. number of simulated draws to return.

- seed:

  optional integer. if supplied, sets a local rng seed and restores the
  user's global [`set.seed()`](https://rdrr.io/r/base/Random.html)
  stream afterward. if `NULL`, simulation uses and advances the current
  rng stream normally.

- model:

  character. one of:

  `"erdos_renyi"`

  :   independent bernoulli edges matched to the observed density (and
      directedness).

  `"configuration"`

  :   configuration-model rewire that preserves the observed degree
      sequence (in/out for directed inputs). uses
      [`igraph::sample_degseq()`](https://r.igraph.org/reference/sample_degseq.html).

  `"dyad_permutation"`

  :   permute dyads (snijders-borgatti vertex relabel + symmetric
      reshuffle). preserves density and, conditional on permutation
      symmetry, degree distribution shape.

- ...:

  passed to the underlying model implementation.

## Value

a list of length `nsim` of netify objects with the same class / mode /
symmetry as the input.

## Author

cassy dorff, shahryar minhas
