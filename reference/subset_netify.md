# internal subset function for netify objects

this is the internal workhorse function called by the s3 method
`subset.netify`. users should typically use
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

  a netify object to subset

- actors:

  character vector of actor names or numeric indices to subset

- time:

  time periods to subset

- layers:

  character vector of layer names to subset from multilayer networks

- from:

  character vector of actor names or numeric indices for actors sending
  ties

- to:

  character vector of actor names or numeric indices for actors
  receiving ties

## Value

a netify object containing the requested subset
