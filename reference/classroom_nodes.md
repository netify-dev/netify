# Synthetic high-school friendship roster (nodes)

A small synthetic roster of 30 students intended for examples and
teaching. Designed to support a typical survey-style workflow: one row
per student, one column per attribute, plus a separate edgelist of
reported friendship ties
([`classroom_edges`](https://netify-dev.github.io/netify/reference/classroom_edges.md)).

## Usage

``` r
data(classroom_nodes)
```

## Format

A data frame with 30 rows and 4 columns:

- `student`:

  Student identifier, character (e.g. `"s01"` .. `"s30"`). Use this as
  the actor column when attaching attributes via
  [`add_node_vars`](https://netify-dev.github.io/netify/reference/add_node_vars.md).

- `gender`:

  Reported gender, character, `"F"` or `"M"`.

- `grade`:

  Grade level, integer 9-12.

- `gpa`:

  Grade point average on the 0-4 scale, numeric.

## Details

This dataset is **synthetic** – generated to illustrate how netify
handles standard student/peer survey data. It is not drawn from any real
classroom. Ties tend to form within the same grade and (more weakly)
within the same gender, so attribute-based analyses such as
[`homophily`](https://netify-dev.github.io/netify/reference/homophily.md)
and
[`mixing_matrix`](https://netify-dev.github.io/netify/reference/mixing_matrix.md)
produce meaningful (non-null) patterns.

Pair with
[`classroom_edges`](https://netify-dev.github.io/netify/reference/classroom_edges.md)
(an undirected friendship edgelist on the same 30 students).

## See also

[`classroom_edges`](https://netify-dev.github.io/netify/reference/classroom_edges.md),
[`netify`](https://netify-dev.github.io/netify/reference/netify.md),
[`netify_workflows`](https://netify-dev.github.io/netify/reference/netify_workflows.md).

## Author

Cassy Dorff, Shahryar Minhas

## Examples

``` r
data(classroom_nodes)
head(classroom_nodes)
#>   student gender grade  gpa
#> 1     s01      F    12 2.66
#> 2     s02      F     9 2.89
#> 3     s03      M    12 2.81
#> 4     s04      M    11 3.08
#> 5     s05      F    12 3.05
#> 6     s06      M    11 3.54
table(classroom_nodes$gender, classroom_nodes$grade)
#>    
#>     9 10 11 12
#>   F 3  3  0  3
#>   M 7  3  5  6
```
