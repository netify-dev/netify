# Melt methods for netify objects

Convert netify matrices/arrays to long format data frames. These methods
provide a consistent interface for melting different types of netify
objects while leveraging C++ for performance.

## Usage

``` r
melt(data, ...)

# S3 method for class 'netify'
melt(
  data,
  ...,
  remove_diagonal = TRUE,
  remove_zeros = TRUE,
  na.rm = TRUE,
  value.name = "value"
)
```

## Arguments

- data:

  A netify object

- ...:

  Additional arguments (see details)

- remove_diagonal:

  Logical. Remove diagonal elements (default: TRUE)

- remove_zeros:

  Logical. Remove zero values (default: TRUE)

- na.rm:

  Logical. Remove NA values (default: TRUE)

- value.name:

  Character. Name for value column (default: "value")

## Value

Data frame with columns: row, col, value (and optionally time/layer)

## Details

The melt method converts netify objects from their matrix representation
to a long format data frame suitable for analysis and visualization. The
output format depends on the type of netify object:

- Cross-sectional: Returns columns row, col, value

- Longitudinal: Returns columns row, col, time, value

- Multilayer: Returns columns row, col, layer, value (and time if
  longitudinal)
