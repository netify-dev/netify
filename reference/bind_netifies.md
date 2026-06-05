# Combine multiple netify objects

`bind_netifies()` concatenates two or more netify objects along the time
axis (for combining cross-sec -\> longit, or stacking two longit panels
into a longer one). all inputs must share the same mode (`unipartite` /
`bipartite`), symmetry, layers, and (for cross-sec inputs) the same
actor set. actor sets across periods may differ – the result is a
`longit_list`.

## Usage

``` r
bind_netifies(
  ...,
  names = NULL,
  align_actors = c("none", "union", "intersection")
)
```

## Arguments

- ...:

  two or more netify objects, or a single list of netify objects.

- names:

  optional character vector to name the resulting periods. if `NULL`,
  periods are auto-named from the inputs' existing period labels (with
  deduplication if collisions).

- align_actors:

  one of `"none"` (default), `"union"`, or `"intersection"`. controls
  how per-period actor sets are reconciled when inputs differ:

  - `"none"`: keep each period's actor set as supplied; resulting
    `longit_list` periods may have different dimensions (matches prior
    behavior).

  - `"union"`: take the union of actor sets across all inputs and pad
    each period with na rows/columns for actors not originally present.

  - `"intersection"`: take the intersection of actor sets across all
    inputs and subset each period to only those actors.

## Value

a `longit_list` netify object.

## Details

for combining different *layers* of the same time slice, use
[`layer_netify()`](https://netify-dev.github.io/netify/reference/layer_netify.md).

this is **not** the same as
[`layer_netify()`](https://netify-dev.github.io/netify/reference/layer_netify.md):

- `bind_netifies()` joins along time.

- [`layer_netify()`](https://netify-dev.github.io/netify/reference/layer_netify.md)
  joins along relation (layer).

nodal and dyadic attributes are concatenated per-period; if two inputs
supply conflicting values for the same (actor, time), the later input
wins (with a one-shot inform).

when downstream models (e.g., `tergm` cmle) require uniform actor
composition across periods, use `align_actors = "union"` or
`"intersection"`.

multilayer inputs are supported when each input has the same layer
labels. the result is a longitudinal list whose per-period elements keep
the layer dimension, so `as.matrix(x, time = ..., layer = ...)`,
[`unnetify()`](https://netify-dev.github.io/netify/reference/unnetify.md),
and
[`decompose_netify()`](https://netify-dev.github.io/netify/reference/decompose_netify.md)
can still address layers.

## Author

cassy dorff, shahryar minhas

## Examples

``` r
if (FALSE) { # \dontrun{
data(icews)
n1 <- netify(icews[icews$year == 2010, ],
actor1 = "i", actor2 = "j", symmetric = FALSE,
weight = "verbCoop")
n2 <- netify(icews[icews$year == 2011, ],
actor1 = "i", actor2 = "j", symmetric = FALSE,
weight = "verbCoop")
combined <- bind_netifies(n1, n2, names = c("2010", "2011"))
summary(combined)
} # }
```
