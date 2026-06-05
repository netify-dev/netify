# Extract actor time range information

`get_actor_time_info` returns a per-actor data.frame of entry and exit
times. it dispatches on the first argument:

## Usage

``` r
get_actor_time_info(x, ...)

# S3 method for class 'netify'
get_actor_time_info(x, ...)

# S3 method for class 'data.frame'
get_actor_time_info(x, actor1, actor2, time, ...)

# Default S3 method
get_actor_time_info(x, actor1, actor2, time, ...)
```

## Arguments

- x:

  a netify object, or a data.frame of dyadic observations.

- ...:

  unused; reserved for future methods.

- actor1:

  character string specifying the column name for the first actor in
  each dyad (data.frame method only).

- actor2:

  character string specifying the column name for the second actor in
  each dyad (data.frame method only).

- time:

  character string specifying the column name for time periods
  (data.frame method only).

## Value

a data.frame with three columns:

- **actor**: character vector of unique actor identifiers.

- **min_time**: earliest time period the actor is in the network (entry
  point).

- **max_time**: latest time period the actor is in the network (exit
  point).

for the netify method, this is a verbatim copy of
`attr(x, "actor_pds")`. for the data.frame method, actors are ordered as
they appear in the aggregation, not alphabetically or by time.

## Details

- if `x` is a **netify object**, it returns the stored `actor_pds`
  attribute directly (one row per actor with `min_time` / `max_time`).
  this is the open-cohort roster the netlet was built with – and the
  roster every per-period statistic (density, degree, homophily) is
  computed against.

- if `x` is a **data.frame** of dyadic observations, it computes the
  entry / exit times from the data. entry is defined as the first time
  period in which an actor appears in any interaction (as either sender
  or receiver), and exit as the last time period. use this form to
  prepare the `actor_pds` argument to
  [`netify()`](https://netify-dev.github.io/netify/reference/netify.md).

**use cases:**

- on a **dyad data.frame**: build the `actor_pds` argument to
  `netify(..., actor_time_uniform = FALSE, actor_pds = ...)` for
  open-cohort panels (panel surveys with attrition, contact-tracing
  chains, organizational membership over time, etc.).

- on a **netify object**: inspect the entry / exit roster the netlet is
  currently using – useful when debugging density denominators, writing
  custom exporters, or verifying that an open-cohort netlet has the
  actor windows you expect.

**assumptions (data.frame method):**

- an actor is considered "present" in any time period where they appear
  in the data, regardless of role (sender/receiver).

- missing values in time are ignored when calculating min/max.

- actors must appear in at least one non-missing time period.

## Note

the data.frame method assumes that presence in the data indicates
network participation. if actors can be temporarily absent from the
network while still being considered members, this method will not
capture such gaps – supply an explicit `actor_pds` roster to
[`netify()`](https://netify-dev.github.io/netify/reference/netify.md)
instead.

## Author

shahryar minhas, ha eun choi

cassy dorff, shahryar minhas

## Examples

``` r
# data.frame input: derive the roster
df <- data.frame(
    i = c("a", "a", "b", "c"),
    j = c("b", "c", "c", "a"),
    t = c(1, 2, 2, 3)
)
get_actor_time_info(df, "i", "j", "t")
#>   actor min_time max_time
#> 1     a        1        3
#> 2     b        1        2
#> 3     c        2        3

# netify input: read back the stored roster
if (FALSE) { # \dontrun{
roster <- data.frame(actor = c("a", "b"), min_time = c(1, 1), max_time = c(3, 4))
net <- netify(df, actor1 = "i", actor2 = "j", time = "t",
              actor_time_uniform = FALSE, actor_pds = roster)
get_actor_time_info(net)
} # }
```
