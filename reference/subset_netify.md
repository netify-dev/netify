# Internal subset function for netify objects

This is the internal workhorse function called by the S3 method
`subset.netify`. Users should typically use
[`subset()`](https://rdrr.io/r/base/subset.html) on netify objects
rather than calling this function directly.

## Usage

``` r
subset_netify(
  netlet,
  actors = NULL,
  time = NULL,
  layers = NULL,
  from = NULL,
  to = NULL
)
```

## Arguments

- netlet:

  A netify object to subset

- actors:

  Character vector of actor names or numeric indices to subset

- time:

  Time periods to subset

- layers:

  Character vector of layer names to subset from multilayer networks

- from:

  Character vector of actor names or numeric indices for actors sending
  ties

- to:

  Character vector of actor names or numeric indices for actors
  receiving ties

## Value

A netify object containing the requested subset
