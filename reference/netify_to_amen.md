# Convert netify objects to amen format

`netify_to_amen` (also available as `to_amen`) transforms netify network
objects into the data structure required by the amen package for
advanced network modeling. This enables the use of Social Relations
Models (SRM), Additive and Multiplicative Effects (AME) models, and
other network regression approaches implemented in amen.

## Usage

``` r
netify_to_amen(netlet, lame = FALSE)

to_amen(netlet, lame = FALSE)
```

## Arguments

- netlet:

  A netify object (class "netify") containing network data. Must be a
  single-layer network. For multilayer networks, first extract
  individual layers using
  [`subset_netify`](https://netify-dev.github.io/netify/reference/subset_netify.md).

- lame:

  Logical. Controls the output format for longitudinal data:

  - `FALSE` (default): Formats output for compatibility with the
    standard version of the amen package, which uses array structures

  - `TRUE`: Formats output for compatibility with the netify-verse
    version called lame, which supports longitudinal network modeling
    with time-varying actor compositions and other features

  This parameter is ignored for cross-sectional data.

## Value

The structure of the returned list depends on the data type and `lame`
parameter:

**For cross-sectional data or longitudinal with lame = FALSE (standard
amen):**

- **Y**:

  Network adjacency data as a numeric matrix or array:

  - Cross-sectional: Matrix of dimensions `[n_actors × n_actors]`

  - Longitudinal: Array of dimensions `[n_actors × n_actors × n_time]`

  Contains edge weights or binary indicators. Missing edges are
  preserved as NA.

- **Xdyad**:

  Dyadic covariates as an array, or NULL if none exist:

  - Cross-sectional: `[n_actors × n_actors × n_covariates]`

  - Longitudinal: `[n_actors × n_actors × n_covariates × n_time]`

  Each slice contains one dyadic covariate across all actor pairs.

- **Xrow**:

  Sender/row actor attributes as a matrix or array, or NULL if none
  exist:

  - Cross-sectional: `[n_actors × n_attributes]`

  - Longitudinal: `[n_actors × n_attributes × n_time]`

  Contains numeric attributes for actors when they act as senders.

- **Xcol**:

  Receiver/column actor attributes, structured identically to Xrow. For
  symmetric networks, Xcol is identical to Xrow. For bipartite networks,
  contains attributes for the second mode.

**For longitudinal data with lame = TRUE (lame package):**

- **Y**:

  A list of length T (time periods), where each element is an n × n
  relational matrix. Actor sets can vary across time periods.

- **Xdyad**:

  A list of length T, where each element is an n × n × pd array of
  dyadic covariates, or NULL if none exist

- **Xrow**:

  A list of length T, where each element is an n × pr matrix of nodal
  row covariates, or NULL if none exist

- **Xcol**:

  A list of length T, where each element is an n × pc matrix of nodal
  column covariates, or NULL if none exist

## Details

**Variable requirements:**

- All nodal attributes must be numeric (integer or double)

- Character or factor variables must be converted before using this
  function

- Missing values (NA) are preserved and can be handled by amen's models

**When to use each format:**

- Use `lame = FALSE` when:

  - Actor composition is constant across time

- Use `lame = TRUE` when:

  - Actors enter/exit the network over time

  - Want access to other features in lame

## Note

The function performs several validation checks:

- Ensures single-layer networks (multilayer not supported)

- Verifies all nodal attributes are numeric

- Maintains actor ordering from the original netify object

## Author

Ha Eun Choi, Cassy Dorff, Colin Henry, Shahryar Minhas

## Examples

``` r
# Load example data
data(icews)

# Create a netify object
net <- netify(
    icews[icews$year == 2010, ],
    actor1 = "i", actor2 = "j",
    symmetric = FALSE,
    weight = "verbCoop"
)

# Convert to amen format (standard)
amen_data <- netify_to_amen(net)
names(amen_data) # Y, Xdyad, Xrow, Xcol
#> [1] "Y"     "Xdyad" "Xrow"  "Xcol" 

if (FALSE) { # \dontrun{
# For longitudinal data with time-varying composition
longit_net <- netify(
    icews,
    actor1 = "i", actor2 = "j", time = "year",
    symmetric = FALSE,
    weight = "verbCoop"
)

# Convert to lame format
lame_data <- netify_to_amen(longit_net, lame = TRUE)
} # }
```
