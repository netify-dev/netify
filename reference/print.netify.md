# Print method for netify objects

Displays a formatted summary of a netify object, including network type,
dimensions, summary statistics, and available attributes.

## Usage

``` r
# S3 method for class 'netify'
print(x, ...)
```

## Arguments

- x:

  A netify object

- ...:

  Additional parameters (not used)

## Value

Invisibly returns the input netify object. Called for its side effect of
printing network information to the console.

## Details

The print method displays:

- Network type (unipartite/bipartite, symmetric/asymmetric)

- Edge weight specification

- Temporal structure (cross-sectional or number of time periods)

- Actor counts (total unique actors, or separate row/column counts for
  bipartite)

- Summary statistics (density, reciprocity, transitivity, etc.)

- Available nodal and dyadic attributes

For longitudinal networks, summary statistics are averaged across time
periods. For multilayer networks, statistics are shown separately for
each layer.

## Author

Ha Eun Choi, Cassy Dorff, Colin Henry, Shahryar Minhas
