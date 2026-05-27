# Simulate null-model networks from a netify object

Generates `nsim` new netify objects from one of three standard null
models, holding the actor set fixed (and, where applicable, the observed
density / degree sequence). Useful for sanity-checking whether an
observed network statistic (transitivity, modularity, etc.) is
surprising relative to a chance benchmark, without reaching for
`statnet::ergm` for a simple null.

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

  A netify object (cross-sectional or per-period; for longitudinal input
  each period is simulated independently).

- nsim:

  Integer. Number of simulated draws to return.

- seed:

  Optional integer. Local RNG seed; the user's global
  [`set.seed()`](https://rdrr.io/r/base/Random.html) stream is left
  untouched.

- model:

  Character. One of:

  `"erdos_renyi"`

  :   Independent Bernoulli edges matched to the observed density (and
      directedness).

  `"configuration"`

  :   Configuration-model rewire that preserves the observed degree
      sequence (in/out for directed inputs). Uses
      [`igraph::sample_degseq()`](https://r.igraph.org/reference/sample_degseq.html).

  `"dyad_permutation"`

  :   Permute dyads (Snijders-Borgatti vertex relabel + symmetric
      reshuffle). Preserves density and, conditional on permutation
      symmetry, degree distribution shape.

- ...:

  Passed to the underlying model implementation.

## Value

A list of length `nsim` of netify objects with the same class / mode /
symmetry as the input.

## Author

Cassy Dorff, Shahryar Minhas
