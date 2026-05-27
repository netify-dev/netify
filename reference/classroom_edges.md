# Synthetic high-school friendship edgelist

A small synthetic edgelist of reported friendships among 30 students
(see
[`classroom_nodes`](https://netify-dev.github.io/netify/reference/classroom_nodes.md)).
Ties are **undirected** – each row records that two students named each
other as friends.

## Usage

``` r
data(classroom_edges)
```

## Format

A data frame with about 50 rows and 2 columns:

- `from`:

  Student identifier of one friend, character.

- `to`:

  Student identifier of the other friend, character.

## Details

The edgelist is synthetic and contains one row per friendship (not two).
When you build a netify object with `symmetric = TRUE` (the default for
undirected ties), the constructor automatically fills in both
directions.

## See also

[`classroom_nodes`](https://netify-dev.github.io/netify/reference/classroom_nodes.md),
[`netify`](https://netify-dev.github.io/netify/reference/netify.md),
[`netify_workflows`](https://netify-dev.github.io/netify/reference/netify_workflows.md).

## Author

Cassy Dorff, Shahryar Minhas

## Examples

``` r
data(classroom_edges)
data(classroom_nodes)
head(classroom_edges)
#>   from  to
#> 1  s01 s03
#> 2  s03 s07
#> 3  s04 s07
#> 4  s06 s07
#> 5  s01 s08
#> 6  s06 s08

# Build a friendship network with student attributes attached.
net <- netify(
    classroom_edges,
    actor1 = "from", actor2 = "to",
    symmetric = TRUE,
    nodal_data = classroom_nodes
)
summary(net)
#>   net num_actors   density num_edges prop_edges_missing competition
#> 1   1         30 0.1172414        51                  0  0.03979239
#>   sd_of_actor_means transitivity
#> 1        0.05249134   0.05769231
```
