# Print method for netify objects

displays a formatted summary of a netify object, including network type,
dimensions, summary statistics, and available attributes.

## Usage

``` r
# S3 method for class 'netify'
print(x, ...)
```

## Arguments

- x:

  a netify object

- ...:

  additional parameters (not used)

## Value

invisibly returns the input netify object. called for its side effect of
printing network information to the console.

## Details

the print method displays:

- network type (unipartite/bipartite, symmetric/asymmetric)

- edge weight specification

- temporal structure (cross-sectional or number of time periods)

- actor counts (total unique actors, or separate row/column counts for
  bipartite)

- summary statistics (density, reciprocity, transitivity, etc.)

- available nodal and dyadic attributes

for longitudinal networks, summary statistics are averaged across time
periods. for multilayer networks, statistics are shown separately for
each layer.

## Author

ha eun choi, cassy dorff, colin henry, shahryar minhas
