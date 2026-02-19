# Print method for netify_comparison objects

Provides a clear, formatted output for network comparison results.
Handles different comparison types (temporal, cross-network, multilayer)
with appropriate formatting.

## Usage

``` r
# S3 method for class 'netify_comparison'
print(x, ..., n = 20)
```

## Arguments

- x:

  A netify_comparison object from compare_networks()

- ...:

  Additional arguments (currently unused)

- n:

  Maximum number of rows to print for summary tables (default 20)

## Value

Invisibly returns the input object

## Examples

``` r
# Compare two networks
data(icews)
net1 <- netify(icews[icews$year == 2010,], actor1 = "i", actor2 = "j")
net2 <- netify(icews[icews$year == 2011,], actor1 = "i", actor2 = "j")
comp <- compare_networks(list("2010" = net1, "2011" = net2))
print(comp)
#> 
#> ── Network Comparison Results ──────────────────────────────────────────────────
#> Comparison type: cross_network
#> Number of networks: 2
#> Comparison focus: edges
#> Method: correlation
#> Algorithm: correlation
#> Permutation type: classic
#> Correlation type: pearson
#> Random seed: 742067701
#> ────────────────────────────────────────────────────────────────────────────────
#> 
#> ── Edge Comparison Summary ──
#> 
#>     comparison correlation
#> 1 2010 vs 2011           1
#> ── Edge Changes 
#> 2010_vs_2011:
#> Added: 0 | Removed: 0 | Maintained: 22952
```
