# Synthetic high-school friendship roster (nodes)

a small synthetic roster of 30 students intended for examples and
teaching. designed to support a typical survey-style workflow: one row
per student, one column per attribute, plus a separate edgelist of
reported friendship ties
([`classroom_edges`](https://netify-dev.github.io/netify/reference/classroom_edges.md)).

## Usage

``` r
data(classroom_nodes)
```

## Format

a data frame with 30 rows and 4 columns:

- `student`:

  student identifier, character (e.g. `"s01"` .. `"s30"`). use this as
  the actor column when attaching attributes via
  [`add_node_vars`](https://netify-dev.github.io/netify/reference/add_node_vars.md).

- `gender`:

  reported gender, character, `"f"` or `"m"`.

- `grade`:

  grade level, integer 9-12.

- `gpa`:

  grade point average on the 0-4 scale, numeric.

## Details

this dataset is **synthetic** – generated to illustrate how netify
handles standard student/peer survey data. it is not drawn from any real
classroom. ties tend to form within the same grade and (more weakly)
within the same gender, so attribute-based analyses such as
[`homophily`](https://netify-dev.github.io/netify/reference/homophily.md)
and
[`mixing_matrix`](https://netify-dev.github.io/netify/reference/mixing_matrix.md)
produce meaningful (non-NULL) patterns.

pair with
[`classroom_edges`](https://netify-dev.github.io/netify/reference/classroom_edges.md)
(an undirected friendship edgelist on the same 30 students).

## See also

[`classroom_edges`](https://netify-dev.github.io/netify/reference/classroom_edges.md),
[`netify`](https://netify-dev.github.io/netify/reference/netify.md),
[`netify_workflows`](https://netify-dev.github.io/netify/reference/netify_workflows.md).

## Author

cassy dorff, shahryar minhas

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
