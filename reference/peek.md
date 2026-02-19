# Preview subsets of network data from netify objects

`peek` provides a convenient way to inspect portions of network data
stored in netify objects. Rather than displaying entire networks (which
can be overwhelming for large datasets), this function allows you to
examine specific actors, time periods, or layers for data exploration,
verification, and debugging.

## Usage

``` r
peek(netlet, actors = NULL, from = 3, to = 3, time = 1, layers = NULL)
```

## Arguments

- netlet:

  A netify object to preview. Can be cross-sectional, longitudinal
  (array or list format), and/or multilayer.

- actors:

  Character vector of actor names or numeric indices to subset. Selects
  these actors as both senders and receivers (extracts subgraph among
  these actors). Overridden by `from` and `to` if specified.

- from:

  Specifies which actors to display as senders of ties (row actors in
  the adjacency matrix). Can be:

  - **Single number**: Shows the first n actors (e.g., `from = 5`
    displays actors 1-5)

  - **Numeric vector**: Shows specific positions (e.g.,
    `from = c(1,3,5)` displays the 1st, 3rd, and 5th actors)

  - **Character vector**: Shows named actors (e.g.,
    `from = c("USA", "China")` displays these specific countries)

  - **NULL**: Shows all row actors (default is 3)

  In bipartite networks, from represents actors in the first mode.

- to:

  Specifies which actors to display as receivers of ties (column actors
  in the adjacency matrix). Accepts the same input types as `from`. In
  bipartite networks, columns represent actors in the second mode.
  Default is 3.

- time:

  For longitudinal networks, specifies which time periods to display.
  Can be:

  - **Single number**: Shows the nth time period (e.g., `time = 1` shows
    the first time period)

  - **Numeric vector**: Shows specific time indices (e.g.,
    `time = c(1,5,10)`)

  - **Character vector**: Shows named time periods (e.g.,
    `time = c("2002", "2006")` displays these specific years)

  - **NULL**: Shows all time periods

  Default is 1 (first time period only). Ignored for cross-sectional
  networks.

- layers:

  For multilayer networks, specifies which layer(s) to display. Must be
  a character vector matching layer names in the netify object (e.g.,
  `layers = c("trade", "alliance")`). For single-layer networks, this
  parameter is ignored. Default is NULL (shows all layers).

## Value

Returns a subset of the raw network data (without netify attributes):

- **Cross-sectional networks**:

  - Single layer: Returns a matrix with selected rows and columns

  - Multilayer: Returns a 3D array (rows × columns × layers)

- **Longitudinal networks**:

  - Array format: Returns an array with dimensions depending on
    selection

  - List format: Returns a list of matrices, one per selected time
    period

All returned objects preserve dimension names (actor names, time labels,
layer names) for easy interpretation. Single dimensions are
automatically dropped.

## Details

**Purpose and Design**

`peek` is designed as a lightweight data exploration tool. Unlike
[`subset_netify`](https://netify-dev.github.io/netify/reference/subset_netify.md),
which creates new netify objects with all attributes preserved, `peek`
returns only the raw network data for quick inspection. This makes it
ideal for:

- Verifying data structure and content

- Checking specific relationships

- Debugging data issues

- Quick visual inspection of network patterns

**Understanding Network Directions**

In directed networks:

- **From** represents actors sending ties (out-ties)

- **To** represents actors receiving ties (in-ties)

- Cell `[i,j]` contains the tie from actor i to actor j

For example, if cell `["USA", "China"]` = 5, this means USA sends a tie
of strength 5 to China.

**Smart Selection Behavior**

The function includes several convenience features:

- Single numbers are expanded to ranges (e.g., `from = 5` becomes first
  5 actors)

- Out-of-bounds indices are silently ignored (no errors during
  exploration)

- Character names are matched to actor labels

- Dimension reduction: if only one time period or layer is selected,
  that dimension is dropped from the output

## Note

**Important distinctions:**

- Use `peek` for quick data inspection (returns raw matrices)

- Use
  [`subset_netify`](https://netify-dev.github.io/netify/reference/subset_netify.md)
  to create new netify objects with attributes

- Use
  [`get_raw`](https://netify-dev.github.io/netify/reference/get_raw.md)
  to extract all raw data from a netify object

When multiple layers are present and no layer selection is specified,
all layers are returned with a warning message to remind you about the
multilayer structure.

## Author

Cassy Dorff, Shahryar Minhas

## Examples

``` r
# Load example data
data(icews)

# Example 1: Basic usage with cross-sectional network
icews_10 <- icews[icews$year == 2010, ]
net <- netify(
    icews_10,
    actor1 = "i", actor2 = "j",
    weight = "verbCoop"
)

# Default: see first 3 actors (both sending and receiving)
peek(net)
#>             Afghanistan Albania Algeria
#> Afghanistan          NA       5       5
#> Albania               5      NA       1
#> Algeria               5       1      NA

# See first 5 senders and first 5 receivers
peek(net, from = 5, to = 5)
#>             Afghanistan Albania Algeria Angola Argentina
#> Afghanistan          NA       5       5      0         2
#> Albania               5      NA       1      0         0
#> Algeria               5       1      NA      6         0
#> Angola                0       0       6     NA         8
#> Argentina             2       0       0      8        NA

# Example 2: Specific actors by name
# See ties from US and China to Russia, India, and Brazil
peek(net,
    from = c("United States", "China"),
    to = c("Russia", "India", "Brazil")
)
#>               India Brazil
#> United States  3324    369
#> China          1209    257

# Use actors parameter to see subgraph
peek(net,
    actors = c("United States", "China", "Russia")
)
#>               United States China
#> United States            NA  4937
#> China                  4937    NA

# Example 3: Longitudinal network
net_longit <- netify(
    icews,
    actor1 = "i", actor2 = "j", time = "year",
    weight = "matlConf"
)

# See first 5 actors in specific years
peek(net_longit,
    from = 5, to = 5,
    time = c("2002", "2006", "2010")
)
#> $`2002`
#>             Afghanistan Albania Algeria Angola Argentina
#> Afghanistan          NA       0       0      0         0
#> Albania               0      NA       0      0         0
#> Algeria               0       0      NA      0         0
#> Angola                0       0       0     NA         0
#> Argentina             0       0       0      0        NA
#> 
#> $`2006`
#>             Afghanistan Albania Algeria Angola Argentina
#> Afghanistan          NA       3       0      0         0
#> Albania               3      NA       1      0         0
#> Algeria               0       1      NA      0         0
#> Angola                0       0       0     NA         0
#> Argentina             0       0       0      0        NA
#> 
#> $`2010`
#>             Afghanistan Albania Algeria Angola Argentina
#> Afghanistan          NA       1       1      0         0
#> Albania               1      NA       0      0         0
#> Algeria               1       0      NA      0         0
#> Angola                0       0       0     NA         1
#> Argentina             0       0       0      1        NA
#> 

# See all actors in year 2010
peek(net_longit,
    from = NULL, to = NULL,
    time = "2010"
)
#> $`2010`
#>                                            Afghanistan Albania Algeria Angola
#> Afghanistan                                         NA       1       1      0
#> Albania                                              1      NA       0      0
#> Algeria                                              1       0      NA      0
#> Angola                                               0       0       0     NA
#> Argentina                                            0       0       0      1
#> Armenia                                              0       0       0      0
#> Australia                                           40       0       0      0
#> Austria                                              1       0       0      0
#> Azerbaijan                                           0       0       0      0
#> Bahrain                                              1       0       0      0
#> Bangladesh                                           3       0       1      0
#> Belarus                                              0       0       0      0
#> Belgium                                              0       0       0      0
#> Benin                                                0       0       0      0
#> Bolivia, Plurinational State Of                      0       0       0      0
#> Bosnia And Herzegovina                               2       0       2      0
#> Brazil                                               0       1       0      1
#> Bulgaria                                             2       0       0      0
#> Burkina Faso                                         0       0       0      0
#> Burundi                                              0       0       0      0
#> Cambodia                                             0       0       0      0
#> Cameroon                                             0       0       0      0
#> Canada                                              19       0       0      0
#> Cape Verde                                           0       0       0      0
#> Central African Republic                             0       0       0      0
#> Chad                                                 0       0       0      0
#> Chile                                                0       0       0      0
#> China                                                3       0       1      0
#> Colombia                                             0       0       0      0
#> Comoros                                              0       0       0      0
#> Congo, Republic Of                                   0       0       0     14
#> Congo, The Democratic Republic Of                    0       0       0      3
#> Costa Rica                                           0       0       0      0
#> Cote D'ivoire                                        0       0       0      0
#> Croatia                                              6       2       0      0
#> Cuba                                                 1       0       1      0
#> Cyprus                                               0       0       0      0
#> Denmark                                             17       0       2      0
#> Djibouti                                             0       0       0      0
#> Dominican Republic                                   0       0       0      0
#> Ecuador                                              0       0       0      0
#> Egypt                                                0       0       3      0
#> El Salvador                                          0       0       0      0
#> Equatorial Guinea                                    0       0       0      0
#> Estonia                                              6       0       0      0
#> Ethiopia                                             1       0       0      0
#> Fiji                                                 0       0       0      0
#> Finland                                              4       0       0      0
#> France                                              48       0      11      2
#> Gabon                                                0       0       0      0
#> Gambia                                               0       0       0      0
#> Georgia                                             14       0       0      0
#> Germany                                             80       2       1      0
#> Ghana                                                0       0       0      1
#> Greece                                               4      36       2      1
#> Guatemala                                            0       0       0      0
#> Guinea                                               0       0       0      0
#> Guinea-Bissau                                        0       0       0      1
#> Guyana                                               0       0       0      0
#> Haiti                                                0       0       0      0
#> Honduras                                             0       0       0      0
#> Hungary                                              4       0       0      0
#> India                                               17       1       0      0
#> Indonesia                                           49       0       1      0
#> Iran, Islamic Republic Of                          141       0       0      0
#> Iraq                                                 4       0       2      0
#> Ireland                                              0       0       0      0
#> Israel                                               5       0       6      0
#> Italy                                               13       1      13      0
#> Jamaica                                              0       0       0      0
#> Japan                                                2       0       0      0
#> Jordan                                              12       0       0      0
#> Kazakhstan                                           4       0       0      0
#> Kenya                                                0       0       0      0
#> Korea, Democratic People's Republic Of               0       0       0      0
#> Korea, Republic Of                                   6       0       0      0
#> Kuwait                                               0       0       0      0
#> Kyrgyzstan                                          12       0       0      0
#> Lao People's Democratic Republic                     0       0       0      0
#> Latvia                                               0       0       0      0
#> Lebanon                                              4       0       0      0
#> Liberia                                              2       0       0      0
#> Libyan Arab Jamahiriya                               0       0       1      0
#> Lithuania                                            2       0       0      0
#> Luxembourg                                           0       0       0      0
#> Macedonia, The Former Yugoslav Republic Of           5       4       0      0
#> Madagascar                                           0       0       0      0
#> Malawi                                               0       0       0      0
#> Malaysia                                             1       0       1      0
#> Mali                                                 0       0       4      0
#> Mauritania                                           1       0      15      0
#> Mauritius                                            0       0       1      0
#> Mexico                                               0       0       0      0
#> Moldova, Republic Of                                 2       0       0      0
#> Mongolia                                             0       0       0      0
#> Morocco                                              0       0       1      0
#> Mozambique                                           0       0       0      0
#> Myanmar                                              0       0       0      0
#> Nepal                                                2       0       0      0
#> Netherlands                                         21       1       0      0
#> New Zealand                                          2       0       0      0
#> Nicaragua                                            0       0       0      0
#> Niger                                                0       0       6      0
#> Nigeria                                              0       0       0      3
#> Norway                                               2       0       0      0
#> Oman                                                 0       0       0      0
#> Pakistan                                           691       0       1      0
#> Panama                                               0       0       0      0
#> Papua New Guinea                                     2       0       0      0
#> Paraguay                                             0       0       0      0
#> Peru                                                 0       0       0      0
#> Philippines                                          3       0       0      0
#> Poland                                              23       0       0      0
#> Portugal                                             2       2       0      0
#> Qatar                                                1       0       0      0
#> Romania                                             15       0       0      0
#> Russian Federation                                  27       0       2      0
#> Rwanda                                               0       0       0      0
#> Saudi Arabia                                         4       0       0      0
#> Senegal                                              0       0       1      0
#> Sierra Leone                                         0       0       0      0
#> Singapore                                            2       0       0      0
#> Slovakia                                            18       1       2      0
#> Slovenia                                             2       0       1      0
#> Solomon Islands                                      0       0       0      0
#> Somalia                                              0       0       2      3
#> South Africa                                         0       0       1      1
#> Spain                                               26       0       2      0
#> Sri Lanka                                            0       0       0      0
#> Sudan                                                0       0       0      0
#> Suriname                                             0       0       0      0
#> Sweden                                               3       0       0      0
#> Switzerland                                          0       0       0      0
#> Syrian Arab Republic                                 0       0       0      0
#> Tajikistan                                          18       0       0      0
#> Tanzania, United Republic Of                         0       0       0      0
#> Thailand                                             0       0       0      0
#> Togo                                                 0       0       2      2
#> Trinidad And Tobago                                  1       0       0      0
#> Tunisia                                              0       0       0      0
#> Turkmenistan                                         0       0       0      0
#> Uganda                                               0       0       0      0
#> Ukraine                                              0       0       0      0
#> United Arab Emirates                                 4       0       0      0
#> United Kingdom                                     116       2       7      1
#> United States                                     1018       1       4      0
#> Uruguay                                              0       0       0      0
#> Uzbekistan                                          10       0       0      0
#> Vietnam                                              0       0       0      0
#> Yemen                                                1       0       0      0
#> Zambia                                               0       0       0      0
#> Zimbabwe                                             0       0       0      0
#>                                            Argentina Armenia Australia Austria
#> Afghanistan                                        0       0        40       1
#> Albania                                            0       0         0       0
#> Algeria                                            0       0         0       0
#> Angola                                             1       0         0       0
#> Argentina                                         NA       1         0       0
#> Armenia                                            1      NA         0       0
#> Australia                                          0       0        NA       0
#> Austria                                            0       0         0      NA
#> Azerbaijan                                         0      50         0       0
#> Bahrain                                            0       0         0       0
#> Bangladesh                                         0       0         0       0
#> Belarus                                            0       0         0       0
#> Belgium                                            0       0         0       0
#> Benin                                              0       0         0       0
#> Bolivia, Plurinational State Of                    4       0         1       0
#> Bosnia And Herzegovina                             0       0         0       1
#> Brazil                                             1       0         0       0
#> Bulgaria                                           0       0         3       0
#> Burkina Faso                                       0       0         0       0
#> Burundi                                            0       0         0       0
#> Cambodia                                           0       0         1       0
#> Cameroon                                           0       0         0       0
#> Canada                                             0       0         9       0
#> Cape Verde                                         0       0         0       0
#> Central African Republic                           0       0         0       0
#> Chad                                               0       0         0       0
#> Chile                                              7       0         0       0
#> China                                              3       0        34       0
#> Colombia                                           1       0         0       0
#> Comoros                                            1       0         0       0
#> Congo, Republic Of                                 0       0         0       0
#> Congo, The Democratic Republic Of                  0       0         0       0
#> Costa Rica                                         0       0         0       0
#> Cote D'ivoire                                      0       0         0       0
#> Croatia                                            0       0         2      65
#> Cuba                                               1       0         0       0
#> Cyprus                                             0       0         1       0
#> Denmark                                            0       0         1       0
#> Djibouti                                           0       0         0       0
#> Dominican Republic                                 1       0         0       0
#> Ecuador                                            0       0         0       0
#> Egypt                                              0       0         5       0
#> El Salvador                                        0       0         0       0
#> Equatorial Guinea                                  0       0         0       0
#> Estonia                                            0       0         0       0
#> Ethiopia                                           0       0         0       0
#> Fiji                                               0       0        25       0
#> Finland                                            0       0         0       0
#> France                                             1       0         4       0
#> Gabon                                              0       0         0       0
#> Gambia                                             0       0         0       0
#> Georgia                                            0       5         0       3
#> Germany                                            8       1         1       2
#> Ghana                                              0       0         0       0
#> Greece                                             2       0         0       4
#> Guatemala                                          0       0         0       0
#> Guinea                                             0       0         0       0
#> Guinea-Bissau                                      0       0         0       0
#> Guyana                                             0       0         0       0
#> Haiti                                              1       0         0       0
#> Honduras                                           2       0         0       0
#> Hungary                                            0       0         3       3
#> India                                              0       0        21       5
#> Indonesia                                          0       0        43       0
#> Iran, Islamic Republic Of                          1       4         2       1
#> Iraq                                               0       0         2       0
#> Ireland                                            0       0         2       0
#> Israel                                             1       2        21       1
#> Italy                                              1       0         2       0
#> Jamaica                                            0       0         0       0
#> Japan                                              1       0        11       0
#> Jordan                                             0       0         1       0
#> Kazakhstan                                         0       0         0       0
#> Kenya                                              0       0         4       0
#> Korea, Democratic People's Republic Of             0       0         3       0
#> Korea, Republic Of                                 0       0         3       0
#> Kuwait                                             0       0         0       0
#> Kyrgyzstan                                         0       0         0       0
#> Lao People's Democratic Republic                   0       0         0       0
#> Latvia                                             0       0         0       0
#> Lebanon                                            2       2         1       0
#> Liberia                                            0       0         0       0
#> Libyan Arab Jamahiriya                             0       0         0       0
#> Lithuania                                          0       0         0       2
#> Luxembourg                                         0       0         0       0
#> Macedonia, The Former Yugoslav Republic Of         0       0         0       0
#> Madagascar                                         0       0         0       0
#> Malawi                                             0       0         0       0
#> Malaysia                                           0       0         7       0
#> Mali                                               0       0         0       0
#> Mauritania                                         0       0         0       0
#> Mauritius                                          0       0         0       0
#> Mexico                                             2       0         3       0
#> Moldova, Republic Of                               0       1         0       0
#> Mongolia                                           0       0         0       0
#> Morocco                                            0       0         0       0
#> Mozambique                                         0       0         0       0
#> Myanmar                                            0       0         5       0
#> Nepal                                              0       0         0       0
#> Netherlands                                        0       0         3       0
#> New Zealand                                        0       0         4       0
#> Nicaragua                                          0       0         0       0
#> Niger                                              0       0         0       0
#> Nigeria                                            0       0         1       0
#> Norway                                             0       0         1       0
#> Oman                                               0       0         0       0
#> Pakistan                                           0       2         2       0
#> Panama                                             0       0         0       0
#> Papua New Guinea                                   0       0         6       0
#> Paraguay                                           3       0         0       0
#> Peru                                               1       0         1       0
#> Philippines                                        1       0        12       0
#> Poland                                             0       0         0       0
#> Portugal                                           1       0         0       0
#> Qatar                                              0       0         0       0
#> Romania                                            0       0         1       0
#> Russian Federation                                 2      13         0       9
#> Rwanda                                             0       0         0       0
#> Saudi Arabia                                       0       0         0       0
#> Senegal                                            0       0         0       0
#> Sierra Leone                                       0       0         0       0
#> Singapore                                          0       0         6       0
#> Slovakia                                           0       0         0       4
#> Slovenia                                           0       0         0       0
#> Solomon Islands                                    0       0         3       0
#> Somalia                                            0       1         2       0
#> South Africa                                       6       0         2       0
#> Spain                                              6       0         0       1
#> Sri Lanka                                          0       0         3       0
#> Sudan                                              0       0         3       0
#> Suriname                                           0       0         0       0
#> Sweden                                             0       0       116       0
#> Switzerland                                        0       0         3       0
#> Syrian Arab Republic                               0       0         0       0
#> Tajikistan                                         0       0         0       0
#> Tanzania, United Republic Of                       0       0         0       0
#> Thailand                                           0       0        50       0
#> Togo                                               0       0         0       0
#> Trinidad And Tobago                                0       0         0       0
#> Tunisia                                            0       0         1       0
#> Turkmenistan                                       0       0         0       0
#> Uganda                                             1       0         0       0
#> Ukraine                                            0       1         0       1
#> United Arab Emirates                               0       0         3       0
#> United Kingdom                                     4       0       103       3
#> United States                                      7       6       142       2
#> Uruguay                                            2       0         0       0
#> Uzbekistan                                         0       0         0       0
#> Vietnam                                            0       0         9       0
#> Yemen                                              0       0        25       0
#> Zambia                                             0       0         0       0
#> Zimbabwe                                           0       0         7       0
#>                                            Azerbaijan Bahrain Bangladesh
#> Afghanistan                                         0       1          3
#> Albania                                             0       0          0
#> Algeria                                             0       0          1
#> Angola                                              0       0          0
#> Argentina                                           0       0          0
#> Armenia                                            50       0          0
#> Australia                                           0       0          0
#> Austria                                             0       0          0
#> Azerbaijan                                         NA       0          2
#> Bahrain                                             0      NA          0
#> Bangladesh                                          2       0         NA
#> Belarus                                             0       0          0
#> Belgium                                             1       0          0
#> Benin                                               0       0          0
#> Bolivia, Plurinational State Of                     0       0          0
#> Bosnia And Herzegovina                              0       0          0
#> Brazil                                              0       0          0
#> Bulgaria                                            0       0          0
#> Burkina Faso                                        0       0          0
#> Burundi                                             0       0          0
#> Cambodia                                            0       0          0
#> Cameroon                                            0       0          0
#> Canada                                              0       0          0
#> Cape Verde                                          0       0          0
#> Central African Republic                            0       0          0
#> Chad                                                0       0          0
#> Chile                                               0       0          0
#> China                                               1       0          0
#> Colombia                                            0       0          0
#> Comoros                                             0       0          0
#> Congo, Republic Of                                  0       0          0
#> Congo, The Democratic Republic Of                   0       0          0
#> Costa Rica                                          0       0          0
#> Cote D'ivoire                                       0       0          0
#> Croatia                                             0       0          0
#> Cuba                                                0       0          0
#> Cyprus                                              0       0          0
#> Denmark                                             0       0          0
#> Djibouti                                            0       0          0
#> Dominican Republic                                  0       0          0
#> Ecuador                                             0       0          0
#> Egypt                                               0       1          0
#> El Salvador                                         0       0          0
#> Equatorial Guinea                                   0       0          0
#> Estonia                                             0       0          0
#> Ethiopia                                            0       0          0
#> Fiji                                                0       0          0
#> Finland                                             0       0          0
#> France                                              1       0          0
#> Gabon                                               0       0          0
#> Gambia                                              0       0          0
#> Georgia                                            11       0          0
#> Germany                                             1       0          0
#> Ghana                                               0       0          0
#> Greece                                              0       0          1
#> Guatemala                                           0       0          0
#> Guinea                                              0       0          0
#> Guinea-Bissau                                       0       0          0
#> Guyana                                              0       0          0
#> Haiti                                               0       0          0
#> Honduras                                            0       0          0
#> Hungary                                             0       0          4
#> India                                               5       1         65
#> Indonesia                                           0       0          3
#> Iran, Islamic Republic Of                          19       2          0
#> Iraq                                                2       0          0
#> Ireland                                             0       0          0
#> Israel                                             13       1          2
#> Italy                                               1       0          0
#> Jamaica                                             0       0          0
#> Japan                                               0       0          1
#> Jordan                                              0       0          0
#> Kazakhstan                                          3       0          0
#> Kenya                                               0       0          0
#> Korea, Democratic People's Republic Of              0       2          0
#> Korea, Republic Of                                  3       0          0
#> Kuwait                                              0       0          0
#> Kyrgyzstan                                          0       0          4
#> Lao People's Democratic Republic                    0       0          0
#> Latvia                                              1       0          0
#> Lebanon                                             2       0          0
#> Liberia                                             0       0          3
#> Libyan Arab Jamahiriya                              0       0          0
#> Lithuania                                           0       0          0
#> Luxembourg                                          0       0          0
#> Macedonia, The Former Yugoslav Republic Of          0       0          0
#> Madagascar                                          0       0          0
#> Malawi                                              0       0          0
#> Malaysia                                            0       0          1
#> Mali                                                0       0          0
#> Mauritania                                          0       0          0
#> Mauritius                                           0       0          0
#> Mexico                                              0       0          0
#> Moldova, Republic Of                                0       0          0
#> Mongolia                                            0       0          0
#> Morocco                                             0       0          0
#> Mozambique                                          0       0          0
#> Myanmar                                             0       0          3
#> Nepal                                               0       0         16
#> Netherlands                                         0       0          0
#> New Zealand                                         0       0          0
#> Nicaragua                                           0       0          0
#> Niger                                               0       0          0
#> Nigeria                                             1       0          3
#> Norway                                              0       0          0
#> Oman                                                0       0          1
#> Pakistan                                            1       0          6
#> Panama                                              0       0          0
#> Papua New Guinea                                    0       0          0
#> Paraguay                                            0       0          0
#> Peru                                                0       0          0
#> Philippines                                         0       0          0
#> Poland                                              0       0          0
#> Portugal                                            0       0          0
#> Qatar                                               0      24          2
#> Romania                                             0       0          1
#> Russian Federation                                 22       0         13
#> Rwanda                                              0       0          2
#> Saudi Arabia                                        0       0          1
#> Senegal                                             0       0          0
#> Sierra Leone                                        0       0          1
#> Singapore                                           0       0          1
#> Slovakia                                            0       0          0
#> Slovenia                                            0       0          0
#> Solomon Islands                                     0       0          0
#> Somalia                                             0       0          0
#> South Africa                                        0       0          1
#> Spain                                               0       0          2
#> Sri Lanka                                           0       1          0
#> Sudan                                               0       0          0
#> Suriname                                            0       0          0
#> Sweden                                              1       0          0
#> Switzerland                                         0       0          0
#> Syrian Arab Republic                                0       2          0
#> Tajikistan                                          0       0          4
#> Tanzania, United Republic Of                        0       0          0
#> Thailand                                            0       0          0
#> Togo                                                0       0          0
#> Trinidad And Tobago                                 0       0          2
#> Tunisia                                             0       0          0
#> Turkmenistan                                        0       0          0
#> Uganda                                              0       0          0
#> Ukraine                                             1       0          0
#> United Arab Emirates                                2       0          2
#> United Kingdom                                      0       2          1
#> United States                                       3       0          3
#> Uruguay                                             0       0          0
#> Uzbekistan                                          0       0          0
#> Vietnam                                             0       0          0
#> Yemen                                               0       0          0
#> Zambia                                              0       0          0
#> Zimbabwe                                            0       0          0
#>                                            Belarus Belgium Benin
#> Afghanistan                                      0       0     0
#> Albania                                          0       0     0
#> Algeria                                          0       0     0
#> Angola                                           0       0     0
#> Argentina                                        0       0     0
#> Armenia                                          0       0     0
#> Australia                                        0       0     0
#> Austria                                          0       0     0
#> Azerbaijan                                       0       1     0
#> Bahrain                                          0       0     0
#> Bangladesh                                       0       0     0
#> Belarus                                         NA       0     0
#> Belgium                                          0      NA     0
#> Benin                                            0       0    NA
#> Bolivia, Plurinational State Of                  0       0     0
#> Bosnia And Herzegovina                           0       0     0
#> Brazil                                           0       0     0
#> Bulgaria                                         0       0     0
#> Burkina Faso                                     0       0     0
#> Burundi                                          0       0     0
#> Cambodia                                         0       0     0
#> Cameroon                                         0       2     1
#> Canada                                           0       1     0
#> Cape Verde                                       0       0     0
#> Central African Republic                         0       0     0
#> Chad                                             0       0     0
#> Chile                                            0       0     0
#> China                                            1       2     0
#> Colombia                                         0       0     0
#> Comoros                                          0       0     0
#> Congo, Republic Of                               0       0     0
#> Congo, The Democratic Republic Of                0       3     0
#> Costa Rica                                       0       0     0
#> Cote D'ivoire                                    0       1     0
#> Croatia                                          0       0     0
#> Cuba                                             0       2     0
#> Cyprus                                           0       0     0
#> Denmark                                          0       1     0
#> Djibouti                                         0       0     0
#> Dominican Republic                               0       0     0
#> Ecuador                                          0       0     0
#> Egypt                                            0       7     0
#> El Salvador                                      0       0     0
#> Equatorial Guinea                                0       0     0
#> Estonia                                          0       1     0
#> Ethiopia                                         0       0     0
#> Fiji                                             0       0     0
#> Finland                                          0       0     0
#> France                                           1       1     0
#> Gabon                                            0       0     0
#> Gambia                                           0       0     0
#> Georgia                                          4       0     0
#> Germany                                          4       1     0
#> Ghana                                            0       0     0
#> Greece                                           0       1     0
#> Guatemala                                        0       0     0
#> Guinea                                           0       0     0
#> Guinea-Bissau                                    0       0     0
#> Guyana                                           0       0     0
#> Haiti                                            0       0     0
#> Honduras                                         0       0     0
#> Hungary                                          0       0     0
#> India                                            1       2     1
#> Indonesia                                        0       0     0
#> Iran, Islamic Republic Of                        4       4     0
#> Iraq                                             0       2     0
#> Ireland                                          0       2     0
#> Israel                                           0       8     0
#> Italy                                            0       0     0
#> Jamaica                                          0       0     0
#> Japan                                           26       0     0
#> Jordan                                           0       0     0
#> Kazakhstan                                       0       0     0
#> Kenya                                            0       0     0
#> Korea, Democratic People's Republic Of           0       0     0
#> Korea, Republic Of                               0       0     0
#> Kuwait                                           0       0     0
#> Kyrgyzstan                                       1       0     0
#> Lao People's Democratic Republic                 0       0     0
#> Latvia                                           0       0     0
#> Lebanon                                          0       0     0
#> Liberia                                          0       0     0
#> Libyan Arab Jamahiriya                           0       0     0
#> Lithuania                                        2       2     0
#> Luxembourg                                       0       0     0
#> Macedonia, The Former Yugoslav Republic Of       0       1     0
#> Madagascar                                       0       0     0
#> Malawi                                           0       0     0
#> Malaysia                                         0       0     0
#> Mali                                             0       0     0
#> Mauritania                                       0       0     0
#> Mauritius                                        0       0     0
#> Mexico                                           0       0     0
#> Moldova, Republic Of                             0       0     0
#> Mongolia                                         0       0     0
#> Morocco                                          0       1     0
#> Mozambique                                       0       0     0
#> Myanmar                                          0       0     0
#> Nepal                                            0       0     0
#> Netherlands                                      0       3     0
#> New Zealand                                      0       0     0
#> Nicaragua                                        0       0     0
#> Niger                                            0       0     2
#> Nigeria                                          0       0     6
#> Norway                                           0       0     0
#> Oman                                             0       0     0
#> Pakistan                                         0       0     0
#> Panama                                           0       0     0
#> Papua New Guinea                                 0       0     0
#> Paraguay                                         0       0     0
#> Peru                                             0       1     0
#> Philippines                                      0       0     0
#> Poland                                          11       2     0
#> Portugal                                         0       0     0
#> Qatar                                            0       0     0
#> Romania                                          0       1     0
#> Russian Federation                              76       9     0
#> Rwanda                                           0       2     0
#> Saudi Arabia                                     0       0     0
#> Senegal                                          0       0     0
#> Sierra Leone                                     0       0     0
#> Singapore                                        0       2     0
#> Slovakia                                         0       0     0
#> Slovenia                                         0       0     0
#> Solomon Islands                                  0       0     0
#> Somalia                                          0       0     0
#> South Africa                                     1       1     0
#> Spain                                            0      15     0
#> Sri Lanka                                        0       0     0
#> Sudan                                            0       1     0
#> Suriname                                         0       0     0
#> Sweden                                           0       0     0
#> Switzerland                                      0       0     0
#> Syrian Arab Republic                             0       0     0
#> Tajikistan                                       0       0     0
#> Tanzania, United Republic Of                     0       0     0
#> Thailand                                         0       1     0
#> Togo                                             0       0     0
#> Trinidad And Tobago                              0       0     0
#> Tunisia                                          0       0     0
#> Turkmenistan                                     0       0     0
#> Uganda                                           0       0     0
#> Ukraine                                          6       0     0
#> United Arab Emirates                             0       0     0
#> United Kingdom                                   0       0     0
#> United States                                   13      10     3
#> Uruguay                                          0       0     0
#> Uzbekistan                                       0       0     0
#> Vietnam                                          0       0     0
#> Yemen                                            0       0     0
#> Zambia                                           0       0     0
#> Zimbabwe                                         0       1     0
#>                                            Bolivia, Plurinational State Of
#> Afghanistan                                                              0
#> Albania                                                                  0
#> Algeria                                                                  0
#> Angola                                                                   0
#> Argentina                                                                4
#> Armenia                                                                  0
#> Australia                                                                1
#> Austria                                                                  0
#> Azerbaijan                                                               0
#> Bahrain                                                                  0
#> Bangladesh                                                               0
#> Belarus                                                                  0
#> Belgium                                                                  0
#> Benin                                                                    0
#> Bolivia, Plurinational State Of                                         NA
#> Bosnia And Herzegovina                                                   0
#> Brazil                                                                   3
#> Bulgaria                                                                 0
#> Burkina Faso                                                             0
#> Burundi                                                                  0
#> Cambodia                                                                 0
#> Cameroon                                                                 0
#> Canada                                                                   0
#> Cape Verde                                                               0
#> Central African Republic                                                 0
#> Chad                                                                     0
#> Chile                                                                    1
#> China                                                                    0
#> Colombia                                                                10
#> Comoros                                                                  0
#> Congo, Republic Of                                                       0
#> Congo, The Democratic Republic Of                                        0
#> Costa Rica                                                               0
#> Cote D'ivoire                                                            0
#> Croatia                                                                  0
#> Cuba                                                                     0
#> Cyprus                                                                   0
#> Denmark                                                                  1
#> Djibouti                                                                 0
#> Dominican Republic                                                       0
#> Ecuador                                                                  0
#> Egypt                                                                    0
#> El Salvador                                                              0
#> Equatorial Guinea                                                        0
#> Estonia                                                                  0
#> Ethiopia                                                                 0
#> Fiji                                                                     0
#> Finland                                                                  0
#> France                                                                   1
#> Gabon                                                                    0
#> Gambia                                                                   0
#> Georgia                                                                  0
#> Germany                                                                  1
#> Ghana                                                                    0
#> Greece                                                                   0
#> Guatemala                                                                0
#> Guinea                                                                   0
#> Guinea-Bissau                                                            0
#> Guyana                                                                   0
#> Haiti                                                                    0
#> Honduras                                                                 1
#> Hungary                                                                  4
#> India                                                                    1
#> Indonesia                                                                0
#> Iran, Islamic Republic Of                                                0
#> Iraq                                                                     0
#> Ireland                                                                  0
#> Israel                                                                   1
#> Italy                                                                    0
#> Jamaica                                                                  0
#> Japan                                                                    0
#> Jordan                                                                   0
#> Kazakhstan                                                               0
#> Kenya                                                                    0
#> Korea, Democratic People's Republic Of                                   0
#> Korea, Republic Of                                                       0
#> Kuwait                                                                   0
#> Kyrgyzstan                                                               0
#> Lao People's Democratic Republic                                         0
#> Latvia                                                                   0
#> Lebanon                                                                  0
#> Liberia                                                                  0
#> Libyan Arab Jamahiriya                                                   0
#> Lithuania                                                                0
#> Luxembourg                                                               0
#> Macedonia, The Former Yugoslav Republic Of                               0
#> Madagascar                                                               0
#> Malawi                                                                   0
#> Malaysia                                                                 0
#> Mali                                                                     0
#> Mauritania                                                               0
#> Mauritius                                                                0
#> Mexico                                                                   4
#> Moldova, Republic Of                                                     0
#> Mongolia                                                                 0
#> Morocco                                                                  0
#> Mozambique                                                               0
#> Myanmar                                                                  0
#> Nepal                                                                    0
#> Netherlands                                                              0
#> New Zealand                                                              0
#> Nicaragua                                                                0
#> Niger                                                                    0
#> Nigeria                                                                  0
#> Norway                                                                   0
#> Oman                                                                     0
#> Pakistan                                                                 0
#> Panama                                                                   0
#> Papua New Guinea                                                         0
#> Paraguay                                                                 3
#> Peru                                                                     6
#> Philippines                                                              0
#> Poland                                                                   0
#> Portugal                                                                 0
#> Qatar                                                                    0
#> Romania                                                                  0
#> Russian Federation                                                       0
#> Rwanda                                                                   0
#> Saudi Arabia                                                             0
#> Senegal                                                                  0
#> Sierra Leone                                                             0
#> Singapore                                                                0
#> Slovakia                                                                 0
#> Slovenia                                                                 0
#> Solomon Islands                                                          0
#> Somalia                                                                  0
#> South Africa                                                             0
#> Spain                                                                    6
#> Sri Lanka                                                                0
#> Sudan                                                                    0
#> Suriname                                                                 0
#> Sweden                                                                   0
#> Switzerland                                                              0
#> Syrian Arab Republic                                                     0
#> Tajikistan                                                               0
#> Tanzania, United Republic Of                                             0
#> Thailand                                                                 0
#> Togo                                                                     0
#> Trinidad And Tobago                                                      0
#> Tunisia                                                                  0
#> Turkmenistan                                                             0
#> Uganda                                                                   0
#> Ukraine                                                                  0
#> United Arab Emirates                                                     0
#> United Kingdom                                                           0
#> United States                                                            5
#> Uruguay                                                                  1
#> Uzbekistan                                                               0
#> Vietnam                                                                  0
#> Yemen                                                                    0
#> Zambia                                                                   0
#> Zimbabwe                                                                 0
#>                                            Bosnia And Herzegovina Brazil
#> Afghanistan                                                     2      0
#> Albania                                                         0      1
#> Algeria                                                         2      0
#> Angola                                                          0      1
#> Argentina                                                       0      1
#> Armenia                                                         0      0
#> Australia                                                       0      0
#> Austria                                                         1      0
#> Azerbaijan                                                      0      0
#> Bahrain                                                         0      0
#> Bangladesh                                                      0      0
#> Belarus                                                         0      0
#> Belgium                                                         0      0
#> Benin                                                           0      0
#> Bolivia, Plurinational State Of                                 0      3
#> Bosnia And Herzegovina                                         NA      0
#> Brazil                                                          0     NA
#> Bulgaria                                                        0      0
#> Burkina Faso                                                    0      0
#> Burundi                                                         0      0
#> Cambodia                                                        0      0
#> Cameroon                                                        0      0
#> Canada                                                          0      7
#> Cape Verde                                                      0      1
#> Central African Republic                                        0      0
#> Chad                                                            0      0
#> Chile                                                           0      1
#> China                                                           3      3
#> Colombia                                                        0      1
#> Comoros                                                         0      0
#> Congo, Republic Of                                              0      0
#> Congo, The Democratic Republic Of                               0      0
#> Costa Rica                                                      0      0
#> Cote D'ivoire                                                   0      1
#> Croatia                                                        11      0
#> Cuba                                                            0      1
#> Cyprus                                                          0      0
#> Denmark                                                         2      0
#> Djibouti                                                        0      0
#> Dominican Republic                                              0      0
#> Ecuador                                                         0      1
#> Egypt                                                           2      0
#> El Salvador                                                     0      1
#> Equatorial Guinea                                               0      0
#> Estonia                                                         0      0
#> Ethiopia                                                        0      0
#> Fiji                                                            0      0
#> Finland                                                         0      0
#> France                                                          0      2
#> Gabon                                                           0      0
#> Gambia                                                          0      0
#> Georgia                                                         0      0
#> Germany                                                         0      7
#> Ghana                                                           0      1
#> Greece                                                          0      0
#> Guatemala                                                       0      1
#> Guinea                                                          0      0
#> Guinea-Bissau                                                   0      0
#> Guyana                                                          0      0
#> Haiti                                                           0      1
#> Honduras                                                        0      3
#> Hungary                                                         1      0
#> India                                                           0      2
#> Indonesia                                                       0      0
#> Iran, Islamic Republic Of                                       0      1
#> Iraq                                                            0      0
#> Ireland                                                         0      0
#> Israel                                                          2      2
#> Italy                                                           0     11
#> Jamaica                                                         0      0
#> Japan                                                           0      0
#> Jordan                                                          0      1
#> Kazakhstan                                                      0      0
#> Kenya                                                           0      0
#> Korea, Democratic People's Republic Of                          1      0
#> Korea, Republic Of                                              0      0
#> Kuwait                                                          0      0
#> Kyrgyzstan                                                      0      0
#> Lao People's Democratic Republic                                0      0
#> Latvia                                                          0      0
#> Lebanon                                                         0      1
#> Liberia                                                         0      0
#> Libyan Arab Jamahiriya                                          0      0
#> Lithuania                                                       0      0
#> Luxembourg                                                      0      0
#> Macedonia, The Former Yugoslav Republic Of                      0      0
#> Madagascar                                                      0      0
#> Malawi                                                          0      0
#> Malaysia                                                        0      0
#> Mali                                                            0      0
#> Mauritania                                                      0      0
#> Mauritius                                                       0      0
#> Mexico                                                          0      8
#> Moldova, Republic Of                                            0      0
#> Mongolia                                                        0      0
#> Morocco                                                         0      0
#> Mozambique                                                      0      0
#> Myanmar                                                         0      0
#> Nepal                                                           0      0
#> Netherlands                                                     1      1
#> New Zealand                                                     0      0
#> Nicaragua                                                       0      0
#> Niger                                                           0      0
#> Nigeria                                                         0      2
#> Norway                                                          0      0
#> Oman                                                            0      0
#> Pakistan                                                        0      1
#> Panama                                                          0      0
#> Papua New Guinea                                                0      0
#> Paraguay                                                        0      7
#> Peru                                                            0      5
#> Philippines                                                     0      1
#> Poland                                                          0      2
#> Portugal                                                        0      1
#> Qatar                                                           0      0
#> Romania                                                         2      0
#> Russian Federation                                              0      1
#> Rwanda                                                          0      0
#> Saudi Arabia                                                    0      0
#> Senegal                                                         0      0
#> Sierra Leone                                                    0      0
#> Singapore                                                       0      0
#> Slovakia                                                        0      0
#> Slovenia                                                        4      0
#> Solomon Islands                                                 0      0
#> Somalia                                                         0      0
#> South Africa                                                    0      2
#> Spain                                                           3      5
#> Sri Lanka                                                       0      0
#> Sudan                                                           0      0
#> Suriname                                                        0      2
#> Sweden                                                          0      1
#> Switzerland                                                     0      1
#> Syrian Arab Republic                                            0      0
#> Tajikistan                                                      0      0
#> Tanzania, United Republic Of                                    0      0
#> Thailand                                                        0      1
#> Togo                                                            0      0
#> Trinidad And Tobago                                             0      0
#> Tunisia                                                         0      0
#> Turkmenistan                                                    0      0
#> Uganda                                                          0      0
#> Ukraine                                                         0      0
#> United Arab Emirates                                            0      0
#> United Kingdom                                                109      5
#> United States                                                   7      3
#> Uruguay                                                         0      1
#> Uzbekistan                                                      0      0
#> Vietnam                                                         0      0
#> Yemen                                                           0      0
#> Zambia                                                          0      0
#> Zimbabwe                                                        0      0
#>                                            Bulgaria Burkina Faso Burundi
#> Afghanistan                                       2            0       0
#> Albania                                           0            0       0
#> Algeria                                           0            0       0
#> Angola                                            0            0       0
#> Argentina                                         0            0       0
#> Armenia                                           0            0       0
#> Australia                                         3            0       0
#> Austria                                           0            0       0
#> Azerbaijan                                        0            0       0
#> Bahrain                                           0            0       0
#> Bangladesh                                        0            0       0
#> Belarus                                           0            0       0
#> Belgium                                           0            0       0
#> Benin                                             0            0       0
#> Bolivia, Plurinational State Of                   0            0       0
#> Bosnia And Herzegovina                            0            0       0
#> Brazil                                            0            0       0
#> Bulgaria                                         NA            0       0
#> Burkina Faso                                      0           NA       0
#> Burundi                                           0            0      NA
#> Cambodia                                          0            0       0
#> Cameroon                                          0            0       0
#> Canada                                            1            0       0
#> Cape Verde                                        0            0       0
#> Central African Republic                          0            0       0
#> Chad                                              0            0       0
#> Chile                                             0            0       0
#> China                                             0            0       0
#> Colombia                                          0            0       0
#> Comoros                                           0            0       0
#> Congo, Republic Of                                0            0       2
#> Congo, The Democratic Republic Of                 1            0       1
#> Costa Rica                                        0            0       0
#> Cote D'ivoire                                     0            1       0
#> Croatia                                           0            0       2
#> Cuba                                              1            0       0
#> Cyprus                                            0            0       0
#> Denmark                                           0            0       0
#> Djibouti                                          0            0       0
#> Dominican Republic                                0            0       0
#> Ecuador                                           0            0       0
#> Egypt                                             0            0       0
#> El Salvador                                       0            0       0
#> Equatorial Guinea                                 0            0       0
#> Estonia                                           0            0       0
#> Ethiopia                                          0            0       0
#> Fiji                                              0            0       0
#> Finland                                           0            0       0
#> France                                            5            6       0
#> Gabon                                             0            0       0
#> Gambia                                            0            0       0
#> Georgia                                           0            0       0
#> Germany                                           2            0       0
#> Ghana                                             0            0       0
#> Greece                                           10            1       0
#> Guatemala                                         0            0       0
#> Guinea                                            0            0       0
#> Guinea-Bissau                                     0            0       0
#> Guyana                                            0            0       0
#> Haiti                                             0            0       0
#> Honduras                                          0            0       0
#> Hungary                                           1            0       0
#> India                                             1            0       0
#> Indonesia                                         0            0       0
#> Iran, Islamic Republic Of                         0            0       0
#> Iraq                                              1            0       0
#> Ireland                                           0            0       0
#> Israel                                            1            0       0
#> Italy                                             0            0       0
#> Jamaica                                           0            0       0
#> Japan                                             3            0       0
#> Jordan                                            0            0       0
#> Kazakhstan                                        0            0       0
#> Kenya                                             0            0       1
#> Korea, Democratic People's Republic Of            0            0       0
#> Korea, Republic Of                                0            0       0
#> Kuwait                                            0            0       0
#> Kyrgyzstan                                        0            0       0
#> Lao People's Democratic Republic                  0            0       0
#> Latvia                                            0            0       0
#> Lebanon                                           0            0       0
#> Liberia                                           0            0       0
#> Libyan Arab Jamahiriya                            0            0       0
#> Lithuania                                         0            0       0
#> Luxembourg                                        0            0       0
#> Macedonia, The Former Yugoslav Republic Of        1            0       0
#> Madagascar                                        0            0       0
#> Malawi                                            0            0       0
#> Malaysia                                          0            0       0
#> Mali                                              0            0       0
#> Mauritania                                        0            0       0
#> Mauritius                                         0            0       0
#> Mexico                                            0            0       0
#> Moldova, Republic Of                              0            0       0
#> Mongolia                                          0            0       0
#> Morocco                                           0            0       0
#> Mozambique                                        0            0       0
#> Myanmar                                           0            0       0
#> Nepal                                             0            0       0
#> Netherlands                                       2            0       0
#> New Zealand                                       0            0       0
#> Nicaragua                                         0            0       0
#> Niger                                             0            0       0
#> Nigeria                                           0            0       0
#> Norway                                            0            0       0
#> Oman                                              0            0       0
#> Pakistan                                          0            0       0
#> Panama                                            0            0       0
#> Papua New Guinea                                  0            0       0
#> Paraguay                                          0            0       0
#> Peru                                              0            0       0
#> Philippines                                       0            0       0
#> Poland                                            0            0       0
#> Portugal                                          0            0       0
#> Qatar                                             0            0       0
#> Romania                                           1            0       0
#> Russian Federation                                2            0       0
#> Rwanda                                            0            0       1
#> Saudi Arabia                                      0            0       0
#> Senegal                                           0            2       0
#> Sierra Leone                                      0            0       0
#> Singapore                                         0            0       0
#> Slovakia                                          0            0       0
#> Slovenia                                          0            0       0
#> Solomon Islands                                   0            0       0
#> Somalia                                           1            0       8
#> South Africa                                      0            0       0
#> Spain                                             0            2       0
#> Sri Lanka                                         0            0       0
#> Sudan                                             0            2       0
#> Suriname                                          0            0       0
#> Sweden                                            1            0       0
#> Switzerland                                       0            0       0
#> Syrian Arab Republic                              0            0       0
#> Tajikistan                                        0            0       0
#> Tanzania, United Republic Of                      2            0       0
#> Thailand                                          0            0       0
#> Togo                                              0            0       0
#> Trinidad And Tobago                               0            0       0
#> Tunisia                                           0            0       0
#> Turkmenistan                                      0            0       0
#> Uganda                                            0            0       2
#> Ukraine                                           0            0       0
#> United Arab Emirates                              0            0       0
#> United Kingdom                                    1            0       0
#> United States                                     4            1       0
#> Uruguay                                           0            0       0
#> Uzbekistan                                        0            0       0
#> Vietnam                                           0            0       0
#> Yemen                                             0            0       0
#> Zambia                                            0            0       0
#> Zimbabwe                                          0            0       1
#>                                            Cambodia Cameroon Canada Cape Verde
#> Afghanistan                                       0        0     19          0
#> Albania                                           0        0      0          0
#> Algeria                                           0        0      0          0
#> Angola                                            0        0      0          0
#> Argentina                                         0        0      0          0
#> Armenia                                           0        0      0          0
#> Australia                                         1        0      9          0
#> Austria                                           0        0      0          0
#> Azerbaijan                                        0        0      0          0
#> Bahrain                                           0        0      0          0
#> Bangladesh                                        0        0      0          0
#> Belarus                                           0        0      0          0
#> Belgium                                           0        2      1          0
#> Benin                                             0        1      0          0
#> Bolivia, Plurinational State Of                   0        0      0          0
#> Bosnia And Herzegovina                            0        0      0          0
#> Brazil                                            0        0      7          1
#> Bulgaria                                          0        0      1          0
#> Burkina Faso                                      0        0      0          0
#> Burundi                                           0        0      0          0
#> Cambodia                                         NA        1      0          0
#> Cameroon                                          1       NA      0          0
#> Canada                                            0        0     NA          0
#> Cape Verde                                        0        0      0         NA
#> Central African Republic                          0        0      0          0
#> Chad                                              0        1      0          0
#> Chile                                             0        0      0          0
#> China                                             4        1      8          0
#> Colombia                                          0        0      0          0
#> Comoros                                           0        0      0          0
#> Congo, Republic Of                                0        0      0          0
#> Congo, The Democratic Republic Of                 0        0      0          0
#> Costa Rica                                        0        0      0          0
#> Cote D'ivoire                                     0        0      1          0
#> Croatia                                           0        0      2          0
#> Cuba                                              0        0      3          0
#> Cyprus                                            0        0     10          0
#> Denmark                                           1        0      0          0
#> Djibouti                                          0        1      0          0
#> Dominican Republic                                0        0      1          0
#> Ecuador                                           0        1      0          0
#> Egypt                                             0        0      1          0
#> El Salvador                                       0        0      0          0
#> Equatorial Guinea                                 0        1      0          0
#> Estonia                                           0        0      0          0
#> Ethiopia                                          0        0      1          0
#> Fiji                                              0        0      0          0
#> Finland                                           0        0      2          0
#> France                                            3        1      2          1
#> Gabon                                             0        2      0          0
#> Gambia                                            0        0      0          0
#> Georgia                                           0        0      0          0
#> Germany                                           0        0      2          0
#> Ghana                                             0        0      0          0
#> Greece                                            0        0      0          0
#> Guatemala                                         0        0      0          0
#> Guinea                                            0        0      0          1
#> Guinea-Bissau                                     0        0      0          0
#> Guyana                                            0        0      0          0
#> Haiti                                             0        0      2          0
#> Honduras                                          0        0      0          0
#> Hungary                                           0        0      0          0
#> India                                             0        0      2          0
#> Indonesia                                         0        0      1          0
#> Iran, Islamic Republic Of                         0        0      6          0
#> Iraq                                              0        1      0          0
#> Ireland                                           0        0      0          0
#> Israel                                            0        0     18          0
#> Italy                                             0        0      1          0
#> Jamaica                                           0        0      0          0
#> Japan                                             3        1      0          0
#> Jordan                                            0        0      0          0
#> Kazakhstan                                        0        0      0          0
#> Kenya                                             0        0      0          0
#> Korea, Democratic People's Republic Of            0        0      3          0
#> Korea, Republic Of                                1        0      0          0
#> Kuwait                                            0        0      0          0
#> Kyrgyzstan                                        0        0      0          0
#> Lao People's Democratic Republic                  1        0      0          0
#> Latvia                                            0        0      0          0
#> Lebanon                                           1        0      0          0
#> Liberia                                           0        0      0          0
#> Libyan Arab Jamahiriya                            0        0      3          0
#> Lithuania                                         0        0      0          0
#> Luxembourg                                        0        0      0          0
#> Macedonia, The Former Yugoslav Republic Of        0        0      0          0
#> Madagascar                                        0        0      0          0
#> Malawi                                            0        0      0          0
#> Malaysia                                          0        0      0          0
#> Mali                                              0        0      0          0
#> Mauritania                                        0        0      0          0
#> Mauritius                                         0        0      0          0
#> Mexico                                            0        0      5          0
#> Moldova, Republic Of                              0        0      0          0
#> Mongolia                                          0        0      0          0
#> Morocco                                           0        0      0          0
#> Mozambique                                        0        0      0          0
#> Myanmar                                           0        3      0          0
#> Nepal                                             0        0      0          0
#> Netherlands                                       0        0      0          0
#> New Zealand                                       2        0      1          0
#> Nicaragua                                         0        0      0          0
#> Niger                                             0        0      0          0
#> Nigeria                                           4        2      1          3
#> Norway                                            1        0      0          0
#> Oman                                              0        0      0          0
#> Pakistan                                          0        0      1          0
#> Panama                                            0        0      0          0
#> Papua New Guinea                                  0        0      0          0
#> Paraguay                                          0        0      0          0
#> Peru                                              0        0      0          0
#> Philippines                                       1        1      0          0
#> Poland                                            0        0      9          0
#> Portugal                                          0        0      0          0
#> Qatar                                             0        0      0          0
#> Romania                                           0        0      0          0
#> Russian Federation                                1        5      1          0
#> Rwanda                                            0        0      2          0
#> Saudi Arabia                                      0        0      2          0
#> Senegal                                           0        0      0          0
#> Sierra Leone                                      0        0      0          0
#> Singapore                                         0        0      0          0
#> Slovakia                                          0        0      0          0
#> Slovenia                                          0        0      0          0
#> Solomon Islands                                   0        0      0          0
#> Somalia                                           0        0      2          0
#> South Africa                                      0        0      1          0
#> Spain                                             0        0      2          0
#> Sri Lanka                                         0        0      2          0
#> Sudan                                             0        0      0          0
#> Suriname                                          0        0      0          0
#> Sweden                                            1        0      1          0
#> Switzerland                                       1        0      0          0
#> Syrian Arab Republic                              0        0      0          0
#> Tajikistan                                        0        0      0          0
#> Tanzania, United Republic Of                      0        0      0          0
#> Thailand                                        139        1      2          0
#> Togo                                              0        0      0          0
#> Trinidad And Tobago                               0        0      0          0
#> Tunisia                                           0        0      0          0
#> Turkmenistan                                      0        0      0          0
#> Uganda                                            0        0      0          0
#> Ukraine                                           0        0      1          0
#> United Arab Emirates                              0        0      3          0
#> United Kingdom                                    2        0      6          0
#> United States                                    11        0     63          3
#> Uruguay                                           0        0      0          0
#> Uzbekistan                                        0        0      0          0
#> Vietnam                                           4        0      1          0
#> Yemen                                             0        0      0          0
#> Zambia                                            0        0      0          0
#> Zimbabwe                                          0        0      0          0
#>                                            Central African Republic Chad Chile
#> Afghanistan                                                       0    0     0
#> Albania                                                           0    0     0
#> Algeria                                                           0    0     0
#> Angola                                                            0    0     0
#> Argentina                                                         0    0     7
#> Armenia                                                           0    0     0
#> Australia                                                         0    0     0
#> Austria                                                           0    0     0
#> Azerbaijan                                                        0    0     0
#> Bahrain                                                           0    0     0
#> Bangladesh                                                        0    0     0
#> Belarus                                                           0    0     0
#> Belgium                                                           0    0     0
#> Benin                                                             0    0     0
#> Bolivia, Plurinational State Of                                   0    0     1
#> Bosnia And Herzegovina                                            0    0     0
#> Brazil                                                            0    0     1
#> Bulgaria                                                          0    0     0
#> Burkina Faso                                                      0    0     0
#> Burundi                                                           0    0     0
#> Cambodia                                                          0    0     0
#> Cameroon                                                          0    1     0
#> Canada                                                            0    0     0
#> Cape Verde                                                        0    0     0
#> Central African Republic                                         NA    6     0
#> Chad                                                              6   NA     0
#> Chile                                                             0    0    NA
#> China                                                             0    1    23
#> Colombia                                                          0    1     1
#> Comoros                                                           0    0     0
#> Congo, Republic Of                                                0    0     0
#> Congo, The Democratic Republic Of                                 4    0     1
#> Costa Rica                                                        0    0     2
#> Cote D'ivoire                                                    13    0     1
#> Croatia                                                           0    0     0
#> Cuba                                                              0    0     1
#> Cyprus                                                            0    0     0
#> Denmark                                                           0    0     0
#> Djibouti                                                          0    0     0
#> Dominican Republic                                                0    0     0
#> Ecuador                                                           0    0     1
#> Egypt                                                             0    0     0
#> El Salvador                                                       0    0     0
#> Equatorial Guinea                                                 0    0     0
#> Estonia                                                           0    0     0
#> Ethiopia                                                          0    0     0
#> Fiji                                                              0    0     0
#> Finland                                                           0    1     0
#> France                                                            0    0     1
#> Gabon                                                             0    0     0
#> Gambia                                                            0    0     0
#> Georgia                                                           0    0     0
#> Germany                                                           0    0     0
#> Ghana                                                             0    0     0
#> Greece                                                            0    0     0
#> Guatemala                                                         0    0     0
#> Guinea                                                            0    1     0
#> Guinea-Bissau                                                     0    0     0
#> Guyana                                                            0    0     0
#> Haiti                                                             0    0     0
#> Honduras                                                          0    0     1
#> Hungary                                                           0    0     0
#> India                                                             1    0     0
#> Indonesia                                                         0    0     0
#> Iran, Islamic Republic Of                                         0    0     0
#> Iraq                                                              0    0     0
#> Ireland                                                           0    0     0
#> Israel                                                            0    0     0
#> Italy                                                             0    0     1
#> Jamaica                                                           0    0     0
#> Japan                                                             0    0     0
#> Jordan                                                            0    0     0
#> Kazakhstan                                                        0    0     0
#> Kenya                                                             0    0     0
#> Korea, Democratic People's Republic Of                            0    0     0
#> Korea, Republic Of                                                0    0     0
#> Kuwait                                                            0    0     0
#> Kyrgyzstan                                                        0    0     0
#> Lao People's Democratic Republic                                  0    0     0
#> Latvia                                                            0    0     0
#> Lebanon                                                           0    0     0
#> Liberia                                                           0    0     0
#> Libyan Arab Jamahiriya                                            0    0     0
#> Lithuania                                                         0    0     0
#> Luxembourg                                                        0    0     0
#> Macedonia, The Former Yugoslav Republic Of                        0    0     0
#> Madagascar                                                        0    0     0
#> Malawi                                                            0    0     0
#> Malaysia                                                          0    0     0
#> Mali                                                              0    0     0
#> Mauritania                                                        0    0     0
#> Mauritius                                                         0    0     0
#> Mexico                                                            0    0     7
#> Moldova, Republic Of                                              0    0     0
#> Mongolia                                                          0    0     0
#> Morocco                                                           0    0     0
#> Mozambique                                                        0    0     0
#> Myanmar                                                           0    0     0
#> Nepal                                                             0    0     0
#> Netherlands                                                       0    0     9
#> New Zealand                                                       0    0     0
#> Nicaragua                                                         0    0     0
#> Niger                                                             0    0     0
#> Nigeria                                                           0    1     0
#> Norway                                                            0    0     0
#> Oman                                                              0    0     0
#> Pakistan                                                          0    0    19
#> Panama                                                            0    0     0
#> Papua New Guinea                                                  0    0     0
#> Paraguay                                                          0    0     1
#> Peru                                                              0    0     8
#> Philippines                                                       0    0     8
#> Poland                                                            0    0     0
#> Portugal                                                          0    0     0
#> Qatar                                                             0    0     0
#> Romania                                                           0    0     0
#> Russian Federation                                                2    0     0
#> Rwanda                                                            0    0     0
#> Saudi Arabia                                                      0    0     0
#> Senegal                                                           0    4     0
#> Sierra Leone                                                      0    0     0
#> Singapore                                                         0    0     0
#> Slovakia                                                         10    0     0
#> Slovenia                                                          0    0     0
#> Solomon Islands                                                   0    0     0
#> Somalia                                                           0    0     0
#> South Africa                                                      0    0     1
#> Spain                                                             0    0     5
#> Sri Lanka                                                         0    0     0
#> Sudan                                                             3   23     0
#> Suriname                                                          0    0     0
#> Sweden                                                            0    0     0
#> Switzerland                                                       0    0     1
#> Syrian Arab Republic                                              0    0     0
#> Tajikistan                                                        0    0     0
#> Tanzania, United Republic Of                                      0    0     0
#> Thailand                                                          0    0     0
#> Togo                                                              1    0     0
#> Trinidad And Tobago                                               0    0     0
#> Tunisia                                                           0    0     0
#> Turkmenistan                                                      0    0     0
#> Uganda                                                           30    1     0
#> Ukraine                                                           0    0     0
#> United Arab Emirates                                              0    0     0
#> United Kingdom                                                    0    0     6
#> United States                                                     1    0    10
#> Uruguay                                                           0    0     3
#> Uzbekistan                                                        0    0     0
#> Vietnam                                                           0    0     0
#> Yemen                                                             0    0     0
#> Zambia                                                            0    0     0
#> Zimbabwe                                                          0    0     0
#>                                            China Colombia Comoros
#> Afghanistan                                    3        0       0
#> Albania                                        0        0       0
#> Algeria                                        1        0       0
#> Angola                                         0        0       0
#> Argentina                                      3        1       1
#> Armenia                                        0        0       0
#> Australia                                     34        0       0
#> Austria                                        0        0       0
#> Azerbaijan                                     1        0       0
#> Bahrain                                        0        0       0
#> Bangladesh                                     0        0       0
#> Belarus                                        1        0       0
#> Belgium                                        2        0       0
#> Benin                                          0        0       0
#> Bolivia, Plurinational State Of                0       10       0
#> Bosnia And Herzegovina                         3        0       0
#> Brazil                                         3        1       0
#> Bulgaria                                       0        0       0
#> Burkina Faso                                   0        0       0
#> Burundi                                        0        0       0
#> Cambodia                                       4        0       0
#> Cameroon                                       1        0       0
#> Canada                                         8        0       0
#> Cape Verde                                     0        0       0
#> Central African Republic                       0        0       0
#> Chad                                           1        1       0
#> Chile                                         23        1       0
#> China                                         NA        0       0
#> Colombia                                       0       NA       0
#> Comoros                                        0        0      NA
#> Congo, Republic Of                             1        0       0
#> Congo, The Democratic Republic Of              0        0       0
#> Costa Rica                                     2        2       0
#> Cote D'ivoire                                  0        0       0
#> Croatia                                        0        0       0
#> Cuba                                           1        2       0
#> Cyprus                                         0        0       0
#> Denmark                                        2        0       0
#> Djibouti                                       0        0       0
#> Dominican Republic                             0        1       0
#> Ecuador                                        0       42       0
#> Egypt                                          0        0       0
#> El Salvador                                    0        0       0
#> Equatorial Guinea                              0        0       0
#> Estonia                                        0        0       0
#> Ethiopia                                       2        0       0
#> Fiji                                           0        0       0
#> Finland                                        0        0       0
#> France                                         3        1       0
#> Gabon                                          1        0       0
#> Gambia                                         0        0       0
#> Georgia                                        0        0       0
#> Germany                                        1        1       0
#> Ghana                                          5        0       0
#> Greece                                         0        0       0
#> Guatemala                                      0        2       0
#> Guinea                                         3        0       0
#> Guinea-Bissau                                  0        0       0
#> Guyana                                         0        0       0
#> Haiti                                          1        0       0
#> Honduras                                       0        1       0
#> Hungary                                        2        0       0
#> India                                         48        5       0
#> Indonesia                                      4        1       0
#> Iran, Islamic Republic Of                      1        0       0
#> Iraq                                           0        0       0
#> Ireland                                        3        0       0
#> Israel                                         0        4       0
#> Italy                                          2        0       0
#> Jamaica                                        0        0       0
#> Japan                                        428        3       0
#> Jordan                                         0        0       0
#> Kazakhstan                                     2        0       0
#> Kenya                                          0        0       0
#> Korea, Democratic People's Republic Of        45        0       0
#> Korea, Republic Of                            23        0       0
#> Kuwait                                         0        0       0
#> Kyrgyzstan                                     4        0       0
#> Lao People's Democratic Republic               0        0       0
#> Latvia                                         0        0       0
#> Lebanon                                        1        1       0
#> Liberia                                        0        0       0
#> Libyan Arab Jamahiriya                         0        0       2
#> Lithuania                                      0        0       0
#> Luxembourg                                     0        0       0
#> Macedonia, The Former Yugoslav Republic Of     0        0       0
#> Madagascar                                     0        0       0
#> Malawi                                         3        0       0
#> Malaysia                                       1        0       0
#> Mali                                           0        0       0
#> Mauritania                                     1        0       0
#> Mauritius                                      0        0       0
#> Mexico                                         4       18       0
#> Moldova, Republic Of                           0        0       0
#> Mongolia                                       4        0       0
#> Morocco                                        0        0       0
#> Mozambique                                     0        0       0
#> Myanmar                                        2        0       0
#> Nepal                                         31        0       0
#> Netherlands                                    0        0       0
#> New Zealand                                    3        0       0
#> Nicaragua                                      0        7       0
#> Niger                                          0        0       0
#> Nigeria                                        5        3       0
#> Norway                                         9        0       0
#> Oman                                           2        0       0
#> Pakistan                                       7        9       0
#> Panama                                         0       15       0
#> Papua New Guinea                               4        0       0
#> Paraguay                                       0        1       0
#> Peru                                           1        2       0
#> Philippines                                   27        1       0
#> Poland                                         1        0       0
#> Portugal                                       0        0       0
#> Qatar                                          0        0       0
#> Romania                                        3        0       0
#> Russian Federation                            19        1       0
#> Rwanda                                         0        0       0
#> Saudi Arabia                                   3        0       0
#> Senegal                                        0        0       0
#> Sierra Leone                                   2        0       0
#> Singapore                                      0        0       0
#> Slovakia                                       0        0       0
#> Slovenia                                       0        0       0
#> Solomon Islands                                0        0       0
#> Somalia                                        2        1       0
#> South Africa                                   7        0       0
#> Spain                                          0       37       0
#> Sri Lanka                                      1        0       0
#> Sudan                                          4        0       0
#> Suriname                                       0        0       0
#> Sweden                                         0        0       0
#> Switzerland                                    1        0       0
#> Syrian Arab Republic                           0        0       0
#> Tajikistan                                     0        0       0
#> Tanzania, United Republic Of                   0        0       2
#> Thailand                                      12        4       0
#> Togo                                           0        0       0
#> Trinidad And Tobago                            0        0       0
#> Tunisia                                        0        0       0
#> Turkmenistan                                   0        0       0
#> Uganda                                         4        0       0
#> Ukraine                                        1        0       0
#> United Arab Emirates                           2        2       0
#> United Kingdom                                 4        3       0
#> United States                                 94       48       0
#> Uruguay                                        0        1       0
#> Uzbekistan                                     2        0       0
#> Vietnam                                        4        0       0
#> Yemen                                         13        0       0
#> Zambia                                         8        0       0
#> Zimbabwe                                       2        0       0
#>                                            Congo, Republic Of
#> Afghanistan                                                 0
#> Albania                                                     0
#> Algeria                                                     0
#> Angola                                                     14
#> Argentina                                                   0
#> Armenia                                                     0
#> Australia                                                   0
#> Austria                                                     0
#> Azerbaijan                                                  0
#> Bahrain                                                     0
#> Bangladesh                                                  0
#> Belarus                                                     0
#> Belgium                                                     0
#> Benin                                                       0
#> Bolivia, Plurinational State Of                             0
#> Bosnia And Herzegovina                                      0
#> Brazil                                                      0
#> Bulgaria                                                    0
#> Burkina Faso                                                0
#> Burundi                                                     2
#> Cambodia                                                    0
#> Cameroon                                                    0
#> Canada                                                      0
#> Cape Verde                                                  0
#> Central African Republic                                    0
#> Chad                                                        0
#> Chile                                                       0
#> China                                                       1
#> Colombia                                                    0
#> Comoros                                                     0
#> Congo, Republic Of                                         NA
#> Congo, The Democratic Republic Of                           4
#> Costa Rica                                                  0
#> Cote D'ivoire                                               0
#> Croatia                                                     0
#> Cuba                                                        0
#> Cyprus                                                      0
#> Denmark                                                     0
#> Djibouti                                                    0
#> Dominican Republic                                          0
#> Ecuador                                                     0
#> Egypt                                                       0
#> El Salvador                                                 0
#> Equatorial Guinea                                           0
#> Estonia                                                     0
#> Ethiopia                                                    0
#> Fiji                                                        0
#> Finland                                                     0
#> France                                                      2
#> Gabon                                                       0
#> Gambia                                                      0
#> Georgia                                                     1
#> Germany                                                     0
#> Ghana                                                       0
#> Greece                                                      0
#> Guatemala                                                   0
#> Guinea                                                      2
#> Guinea-Bissau                                               0
#> Guyana                                                      0
#> Haiti                                                       0
#> Honduras                                                    0
#> Hungary                                                     0
#> India                                                       7
#> Indonesia                                                   0
#> Iran, Islamic Republic Of                                   0
#> Iraq                                                        0
#> Ireland                                                     0
#> Israel                                                      0
#> Italy                                                       0
#> Jamaica                                                     0
#> Japan                                                       0
#> Jordan                                                      0
#> Kazakhstan                                                  0
#> Kenya                                                       0
#> Korea, Democratic People's Republic Of                      0
#> Korea, Republic Of                                          0
#> Kuwait                                                      0
#> Kyrgyzstan                                                  0
#> Lao People's Democratic Republic                            0
#> Latvia                                                      0
#> Lebanon                                                     0
#> Liberia                                                     0
#> Libyan Arab Jamahiriya                                      0
#> Lithuania                                                   0
#> Luxembourg                                                  0
#> Macedonia, The Former Yugoslav Republic Of                  0
#> Madagascar                                                  0
#> Malawi                                                      0
#> Malaysia                                                    0
#> Mali                                                        0
#> Mauritania                                                  0
#> Mauritius                                                   0
#> Mexico                                                      0
#> Moldova, Republic Of                                        0
#> Mongolia                                                    1
#> Morocco                                                     0
#> Mozambique                                                  0
#> Myanmar                                                     0
#> Nepal                                                       0
#> Netherlands                                                 0
#> New Zealand                                                 0
#> Nicaragua                                                   0
#> Niger                                                       0
#> Nigeria                                                     1
#> Norway                                                      1
#> Oman                                                        0
#> Pakistan                                                    0
#> Panama                                                      0
#> Papua New Guinea                                            0
#> Paraguay                                                    0
#> Peru                                                        0
#> Philippines                                                 0
#> Poland                                                      0
#> Portugal                                                    0
#> Qatar                                                       0
#> Romania                                                     0
#> Russian Federation                                          0
#> Rwanda                                                     24
#> Saudi Arabia                                                0
#> Senegal                                                     0
#> Sierra Leone                                                0
#> Singapore                                                   0
#> Slovakia                                                    0
#> Slovenia                                                    0
#> Solomon Islands                                             0
#> Somalia                                                     0
#> South Africa                                                1
#> Spain                                                       0
#> Sri Lanka                                                   0
#> Sudan                                                       0
#> Suriname                                                    0
#> Sweden                                                      0
#> Switzerland                                                 0
#> Syrian Arab Republic                                        0
#> Tajikistan                                                  0
#> Tanzania, United Republic Of                                0
#> Thailand                                                    0
#> Togo                                                        0
#> Trinidad And Tobago                                         0
#> Tunisia                                                     0
#> Turkmenistan                                                0
#> Uganda                                                     18
#> Ukraine                                                     0
#> United Arab Emirates                                        0
#> United Kingdom                                              0
#> United States                                               1
#> Uruguay                                                     0
#> Uzbekistan                                                  0
#> Vietnam                                                     0
#> Yemen                                                       0
#> Zambia                                                      0
#> Zimbabwe                                                    1
#>                                            Congo, The Democratic Republic Of
#> Afghanistan                                                                0
#> Albania                                                                    0
#> Algeria                                                                    0
#> Angola                                                                     3
#> Argentina                                                                  0
#> Armenia                                                                    0
#> Australia                                                                  0
#> Austria                                                                    0
#> Azerbaijan                                                                 0
#> Bahrain                                                                    0
#> Bangladesh                                                                 0
#> Belarus                                                                    0
#> Belgium                                                                    3
#> Benin                                                                      0
#> Bolivia, Plurinational State Of                                            0
#> Bosnia And Herzegovina                                                     0
#> Brazil                                                                     0
#> Bulgaria                                                                   1
#> Burkina Faso                                                               0
#> Burundi                                                                    1
#> Cambodia                                                                   0
#> Cameroon                                                                   0
#> Canada                                                                     0
#> Cape Verde                                                                 0
#> Central African Republic                                                   4
#> Chad                                                                       0
#> Chile                                                                      1
#> China                                                                      0
#> Colombia                                                                   0
#> Comoros                                                                    0
#> Congo, Republic Of                                                         4
#> Congo, The Democratic Republic Of                                         NA
#> Costa Rica                                                                 1
#> Cote D'ivoire                                                              0
#> Croatia                                                                    0
#> Cuba                                                                       0
#> Cyprus                                                                     0
#> Denmark                                                                    0
#> Djibouti                                                                   0
#> Dominican Republic                                                         0
#> Ecuador                                                                    0
#> Egypt                                                                      0
#> El Salvador                                                                0
#> Equatorial Guinea                                                          0
#> Estonia                                                                    0
#> Ethiopia                                                                   0
#> Fiji                                                                       0
#> Finland                                                                    0
#> France                                                                     2
#> Gabon                                                                      0
#> Gambia                                                                     0
#> Georgia                                                                    0
#> Germany                                                                    0
#> Ghana                                                                      0
#> Greece                                                                     0
#> Guatemala                                                                  0
#> Guinea                                                                     0
#> Guinea-Bissau                                                              0
#> Guyana                                                                     0
#> Haiti                                                                      0
#> Honduras                                                                   0
#> Hungary                                                                    0
#> India                                                                      2
#> Indonesia                                                                  0
#> Iran, Islamic Republic Of                                                  0
#> Iraq                                                                       0
#> Ireland                                                                    0
#> Israel                                                                     0
#> Italy                                                                      0
#> Jamaica                                                                    0
#> Japan                                                                      0
#> Jordan                                                                     0
#> Kazakhstan                                                                 0
#> Kenya                                                                      0
#> Korea, Democratic People's Republic Of                                     0
#> Korea, Republic Of                                                         0
#> Kuwait                                                                     0
#> Kyrgyzstan                                                                 0
#> Lao People's Democratic Republic                                           0
#> Latvia                                                                     0
#> Lebanon                                                                    0
#> Liberia                                                                    0
#> Libyan Arab Jamahiriya                                                     0
#> Lithuania                                                                  0
#> Luxembourg                                                                 0
#> Macedonia, The Former Yugoslav Republic Of                                 0
#> Madagascar                                                                 0
#> Malawi                                                                     0
#> Malaysia                                                                   0
#> Mali                                                                       0
#> Mauritania                                                                 0
#> Mauritius                                                                  0
#> Mexico                                                                     0
#> Moldova, Republic Of                                                       0
#> Mongolia                                                                   0
#> Morocco                                                                    0
#> Mozambique                                                                 0
#> Myanmar                                                                    0
#> Nepal                                                                      0
#> Netherlands                                                                1
#> New Zealand                                                                0
#> Nicaragua                                                                  0
#> Niger                                                                      0
#> Nigeria                                                                    0
#> Norway                                                                     8
#> Oman                                                                       0
#> Pakistan                                                                   0
#> Panama                                                                     0
#> Papua New Guinea                                                           0
#> Paraguay                                                                   0
#> Peru                                                                       0
#> Philippines                                                                0
#> Poland                                                                     0
#> Portugal                                                                   0
#> Qatar                                                                      0
#> Romania                                                                    0
#> Russian Federation                                                         0
#> Rwanda                                                                    31
#> Saudi Arabia                                                               0
#> Senegal                                                                    0
#> Sierra Leone                                                               0
#> Singapore                                                                  0
#> Slovakia                                                                   0
#> Slovenia                                                                   0
#> Solomon Islands                                                            0
#> Somalia                                                                    0
#> South Africa                                                               0
#> Spain                                                                      3
#> Sri Lanka                                                                  0
#> Sudan                                                                      2
#> Suriname                                                                   0
#> Sweden                                                                     0
#> Switzerland                                                                0
#> Syrian Arab Republic                                                       0
#> Tajikistan                                                                 0
#> Tanzania, United Republic Of                                               0
#> Thailand                                                                   0
#> Togo                                                                       0
#> Trinidad And Tobago                                                        0
#> Tunisia                                                                    0
#> Turkmenistan                                                               0
#> Uganda                                                                    13
#> Ukraine                                                                    1
#> United Arab Emirates                                                       0
#> United Kingdom                                                             0
#> United States                                                              2
#> Uruguay                                                                    0
#> Uzbekistan                                                                 0
#> Vietnam                                                                    0
#> Yemen                                                                      0
#> Zambia                                                                     0
#> Zimbabwe                                                                   0
#>                                            Costa Rica Cote D'ivoire Croatia
#> Afghanistan                                         0             0       6
#> Albania                                             0             0       2
#> Algeria                                             0             0       0
#> Angola                                              0             0       0
#> Argentina                                           0             0       0
#> Armenia                                             0             0       0
#> Australia                                           0             0       2
#> Austria                                             0             0      65
#> Azerbaijan                                          0             0       0
#> Bahrain                                             0             0       0
#> Bangladesh                                          0             0       0
#> Belarus                                             0             0       0
#> Belgium                                             0             1       0
#> Benin                                               0             0       0
#> Bolivia, Plurinational State Of                     0             0       0
#> Bosnia And Herzegovina                              0             0      11
#> Brazil                                              0             1       0
#> Bulgaria                                            0             0       0
#> Burkina Faso                                        0             1       0
#> Burundi                                             0             0       2
#> Cambodia                                            0             0       0
#> Cameroon                                            0             0       0
#> Canada                                              0             1       2
#> Cape Verde                                          0             0       0
#> Central African Republic                            0            13       0
#> Chad                                                0             0       0
#> Chile                                               2             1       0
#> China                                               2             0       0
#> Colombia                                            2             0       0
#> Comoros                                             0             0       0
#> Congo, Republic Of                                  0             0       0
#> Congo, The Democratic Republic Of                   1             0       0
#> Costa Rica                                         NA             0       0
#> Cote D'ivoire                                       0            NA       0
#> Croatia                                             0             0      NA
#> Cuba                                                0             0       0
#> Cyprus                                              0             0       0
#> Denmark                                             0             0       0
#> Djibouti                                            0             0       0
#> Dominican Republic                                  0             0       0
#> Ecuador                                             0             0       0
#> Egypt                                               0             1       0
#> El Salvador                                         0             0       0
#> Equatorial Guinea                                   0             0       0
#> Estonia                                             0             0       0
#> Ethiopia                                            0             0       0
#> Fiji                                                0             0       0
#> Finland                                             0             0       0
#> France                                              0             6       0
#> Gabon                                               0             0       0
#> Gambia                                              0             0       0
#> Georgia                                             0             0       0
#> Germany                                             2             0       0
#> Ghana                                               0             0       0
#> Greece                                              0             0       0
#> Guatemala                                           1             0       0
#> Guinea                                              0             0       0
#> Guinea-Bissau                                       0             0       0
#> Guyana                                              0             0       0
#> Haiti                                               0             1       1
#> Honduras                                            0             0       0
#> Hungary                                             0             0       0
#> India                                               0             0       0
#> Indonesia                                           0             0       0
#> Iran, Islamic Republic Of                           0             0       0
#> Iraq                                                0             0       3
#> Ireland                                             0             0       0
#> Israel                                              1             0       0
#> Italy                                               0             1       0
#> Jamaica                                             0             0       0
#> Japan                                               0             1       0
#> Jordan                                              0             0       0
#> Kazakhstan                                          0             0       0
#> Kenya                                               0             0       0
#> Korea, Democratic People's Republic Of              0             0       1
#> Korea, Republic Of                                  1             0       0
#> Kuwait                                              0             0       0
#> Kyrgyzstan                                          0             0       0
#> Lao People's Democratic Republic                    0             0       0
#> Latvia                                              0             0       0
#> Lebanon                                             0             0       0
#> Liberia                                             0             1       0
#> Libyan Arab Jamahiriya                              0             0       0
#> Lithuania                                           0             0       0
#> Luxembourg                                          0             0       0
#> Macedonia, The Former Yugoslav Republic Of          0             0       0
#> Madagascar                                          0             0       0
#> Malawi                                              0             0       0
#> Malaysia                                            0             0       0
#> Mali                                                0             0       0
#> Mauritania                                          0             0       0
#> Mauritius                                           0             0       0
#> Mexico                                              1             0       0
#> Moldova, Republic Of                                0             0       0
#> Mongolia                                            1             0       0
#> Morocco                                             0             0       0
#> Mozambique                                          0             0       0
#> Myanmar                                             0             0       0
#> Nepal                                               0             0       0
#> Netherlands                                         0             0       2
#> New Zealand                                         0             0       0
#> Nicaragua                                           5             0       0
#> Niger                                               0             1       0
#> Nigeria                                             0             0       3
#> Norway                                              0             0       1
#> Oman                                                0             0       0
#> Pakistan                                            0             0       0
#> Panama                                              2             0       0
#> Papua New Guinea                                    0             0       0
#> Paraguay                                            0             0       0
#> Peru                                                0             0       0
#> Philippines                                         0             0       1
#> Poland                                              0             0       0
#> Portugal                                            0             1       0
#> Qatar                                               0             0       0
#> Romania                                             0             0       1
#> Russian Federation                                  0             0       0
#> Rwanda                                              0             0       0
#> Saudi Arabia                                        0             0       0
#> Senegal                                             0             1       0
#> Sierra Leone                                        0             0       0
#> Singapore                                           0             0       0
#> Slovakia                                            0             0       0
#> Slovenia                                            0             0       3
#> Solomon Islands                                     0             0       0
#> Somalia                                             1             2       1
#> South Africa                                        0             1       0
#> Spain                                               0             0       0
#> Sri Lanka                                           0             0       0
#> Sudan                                               0             0       0
#> Suriname                                            0             0       0
#> Sweden                                              0             0       0
#> Switzerland                                         0             0       0
#> Syrian Arab Republic                                0             0       1
#> Tajikistan                                          0             0       2
#> Tanzania, United Republic Of                        0             0       0
#> Thailand                                            0             0       0
#> Togo                                                0             0       0
#> Trinidad And Tobago                                 0             0       0
#> Tunisia                                             0             0       0
#> Turkmenistan                                        0             0       0
#> Uganda                                              0             0       0
#> Ukraine                                             0             1       0
#> United Arab Emirates                                0             2       0
#> United Kingdom                                      0             0      13
#> United States                                       1            10       2
#> Uruguay                                             0             0       0
#> Uzbekistan                                          0             0       0
#> Vietnam                                             0             0       0
#> Yemen                                               0             0       0
#> Zambia                                              0             0       0
#> Zimbabwe                                            0             1       0
#>                                            Cuba Cyprus Denmark Djibouti
#> Afghanistan                                   1      0      17        0
#> Albania                                       0      0       0        0
#> Algeria                                       1      0       2        0
#> Angola                                        0      0       0        0
#> Argentina                                     1      0       0        0
#> Armenia                                       0      0       0        0
#> Australia                                     0      1       1        0
#> Austria                                       0      0       0        0
#> Azerbaijan                                    0      0       0        0
#> Bahrain                                       0      0       0        0
#> Bangladesh                                    0      0       0        0
#> Belarus                                       0      0       0        0
#> Belgium                                       2      0       1        0
#> Benin                                         0      0       0        0
#> Bolivia, Plurinational State Of               0      0       1        0
#> Bosnia And Herzegovina                        0      0       2        0
#> Brazil                                        1      0       0        0
#> Bulgaria                                      1      0       0        0
#> Burkina Faso                                  0      0       0        0
#> Burundi                                       0      0       0        0
#> Cambodia                                      0      0       1        0
#> Cameroon                                      0      0       0        1
#> Canada                                        3     10       0        0
#> Cape Verde                                    0      0       0        0
#> Central African Republic                      0      0       0        0
#> Chad                                          0      0       0        0
#> Chile                                         1      0       0        0
#> China                                         1      0       2        0
#> Colombia                                      2      0       0        0
#> Comoros                                       0      0       0        0
#> Congo, Republic Of                            0      0       0        0
#> Congo, The Democratic Republic Of             0      0       0        0
#> Costa Rica                                    0      0       0        0
#> Cote D'ivoire                                 0      0       0        0
#> Croatia                                       0      0       0        0
#> Cuba                                         NA      0       0        0
#> Cyprus                                        0     NA       0        0
#> Denmark                                       0      0      NA        0
#> Djibouti                                      0      0       0       NA
#> Dominican Republic                            1      0       0        0
#> Ecuador                                       0      0       0        0
#> Egypt                                         0      0       0        0
#> El Salvador                                   2      0       0        0
#> Equatorial Guinea                             0      0       0        0
#> Estonia                                       0      0       0        0
#> Ethiopia                                      0      0       0        1
#> Fiji                                          0      0       0        0
#> Finland                                       0      0       0        0
#> France                                        0      0       0        0
#> Gabon                                         0      0       0        0
#> Gambia                                        0      0       0        0
#> Georgia                                       0      0       0        0
#> Germany                                       0      0       1        1
#> Ghana                                         0      0       0        0
#> Greece                                        0      1       0        1
#> Guatemala                                     1      0       0        0
#> Guinea                                        0      0       0        0
#> Guinea-Bissau                                 0      0       0        0
#> Guyana                                        0      0       0        0
#> Haiti                                         1      0       0        0
#> Honduras                                      0      0       0        0
#> Hungary                                       0      0       0        0
#> India                                         0      0       4        0
#> Indonesia                                     0      0       0        0
#> Iran, Islamic Republic Of                     0      0       3        0
#> Iraq                                          0      0       1        0
#> Ireland                                       0      0       0        0
#> Israel                                        3      2       0        0
#> Italy                                         0      0       0        0
#> Jamaica                                       1      0       0        0
#> Japan                                         0      0       0        5
#> Jordan                                        0      0       2        0
#> Kazakhstan                                    0      0       0        0
#> Kenya                                         0      0       0        0
#> Korea, Democratic People's Republic Of        0      0       0        0
#> Korea, Republic Of                            1      0       0        0
#> Kuwait                                        1      0       0        0
#> Kyrgyzstan                                    0      0       0        0
#> Lao People's Democratic Republic              0      0       0        0
#> Latvia                                        0      0       0        0
#> Lebanon                                       0      0       1        0
#> Liberia                                       0      0       0        0
#> Libyan Arab Jamahiriya                        1      0       0        0
#> Lithuania                                     0      0       0        0
#> Luxembourg                                    0      0       1        0
#> Macedonia, The Former Yugoslav Republic Of    0      0       0        0
#> Madagascar                                    0      0       0        0
#> Malawi                                        0      0       0        0
#> Malaysia                                      0      0       0        0
#> Mali                                          0      0       0        0
#> Mauritania                                    0      0       0        0
#> Mauritius                                     0      0       0        0
#> Mexico                                        1      0       0        0
#> Moldova, Republic Of                          0      0       0        0
#> Mongolia                                      0      0       0        0
#> Morocco                                       0      0       0        0
#> Mozambique                                    0      0       0        0
#> Myanmar                                       0      0       0        0
#> Nepal                                         0      0       0        0
#> Netherlands                                   0      0       1        0
#> New Zealand                                   0      0       0        0
#> Nicaragua                                     1      0       2        0
#> Niger                                         0      0       0        0
#> Nigeria                                       0      0       0        0
#> Norway                                        0      0       0        0
#> Oman                                          0      0       0        0
#> Pakistan                                      0      1       2        0
#> Panama                                        1      0       0        0
#> Papua New Guinea                              0      0       0        0
#> Paraguay                                      0      0       0        0
#> Peru                                          0      0       0        0
#> Philippines                                   0      0       0        0
#> Poland                                        0      0       0        0
#> Portugal                                      0      0       0        0
#> Qatar                                         0      0       0        0
#> Romania                                       0      3       0        0
#> Russian Federation                            0      7       1        0
#> Rwanda                                        0      0       0        0
#> Saudi Arabia                                  2      0       2        0
#> Senegal                                       0      0       0        0
#> Sierra Leone                                  0      0       0        0
#> Singapore                                     0      0       0        0
#> Slovakia                                      0      0       0        0
#> Slovenia                                      0      0       1        0
#> Solomon Islands                               0      0       0        0
#> Somalia                                       0      0      17        1
#> South Africa                                  0      0       1        0
#> Spain                                         2      0       0        1
#> Sri Lanka                                     0      0       0        0
#> Sudan                                         0      0       0        0
#> Suriname                                      0      0       0        0
#> Sweden                                        0      0       1        0
#> Switzerland                                   0      0       0        0
#> Syrian Arab Republic                          2      0       0        0
#> Tajikistan                                    0      0       0        0
#> Tanzania, United Republic Of                  0      0       0        0
#> Thailand                                      0      0       0        0
#> Togo                                          0      0       0        0
#> Trinidad And Tobago                           0      0       0        0
#> Tunisia                                       1      0       0        0
#> Turkmenistan                                  0      0       0        0
#> Uganda                                        0      0       1        0
#> Ukraine                                       0      0       0        0
#> United Arab Emirates                          0      0       0        0
#> United Kingdom                                0      1       1        0
#> United States                                97      2       3        0
#> Uruguay                                       1      0       0        0
#> Uzbekistan                                    0      0       0        0
#> Vietnam                                       0      0       0        0
#> Yemen                                         1      0       0        3
#> Zambia                                        0      0       0        0
#> Zimbabwe                                      1      0       0        0
#>                                            Dominican Republic Ecuador Egypt
#> Afghanistan                                                 0       0     0
#> Albania                                                     0       0     0
#> Algeria                                                     0       0     3
#> Angola                                                      0       0     0
#> Argentina                                                   1       0     0
#> Armenia                                                     0       0     0
#> Australia                                                   0       0     5
#> Austria                                                     0       0     0
#> Azerbaijan                                                  0       0     0
#> Bahrain                                                     0       0     1
#> Bangladesh                                                  0       0     0
#> Belarus                                                     0       0     0
#> Belgium                                                     0       0     7
#> Benin                                                       0       0     0
#> Bolivia, Plurinational State Of                             0       0     0
#> Bosnia And Herzegovina                                      0       0     2
#> Brazil                                                      0       1     0
#> Bulgaria                                                    0       0     0
#> Burkina Faso                                                0       0     0
#> Burundi                                                     0       0     0
#> Cambodia                                                    0       0     0
#> Cameroon                                                    0       1     0
#> Canada                                                      1       0     1
#> Cape Verde                                                  0       0     0
#> Central African Republic                                    0       0     0
#> Chad                                                        0       0     0
#> Chile                                                       0       1     0
#> China                                                       0       0     0
#> Colombia                                                    1      42     0
#> Comoros                                                     0       0     0
#> Congo, Republic Of                                          0       0     0
#> Congo, The Democratic Republic Of                           0       0     0
#> Costa Rica                                                  0       0     0
#> Cote D'ivoire                                               0       0     1
#> Croatia                                                     0       0     0
#> Cuba                                                        1       0     0
#> Cyprus                                                      0       0     0
#> Denmark                                                     0       0     0
#> Djibouti                                                    0       0     0
#> Dominican Republic                                         NA       0     0
#> Ecuador                                                     0      NA     0
#> Egypt                                                       0       0    NA
#> El Salvador                                                 1       1     0
#> Equatorial Guinea                                           0       0     0
#> Estonia                                                     0       0     0
#> Ethiopia                                                    0       0     2
#> Fiji                                                        0       0     0
#> Finland                                                     0       0     0
#> France                                                      1       1     6
#> Gabon                                                       0       0     0
#> Gambia                                                      0       0     0
#> Georgia                                                     0       0     0
#> Germany                                                     0       0     1
#> Ghana                                                       0       0     0
#> Greece                                                      0       0     1
#> Guatemala                                                   0       2     0
#> Guinea                                                      0       0     0
#> Guinea-Bissau                                               0       0     0
#> Guyana                                                      0       0     0
#> Haiti                                                       6       0     0
#> Honduras                                                    1       1     0
#> Hungary                                                     0       0     0
#> India                                                       1       4     3
#> Indonesia                                                   0       0     1
#> Iran, Islamic Republic Of                                   0       0     6
#> Iraq                                                        0       0     9
#> Ireland                                                     0       0     0
#> Israel                                                      0       0    41
#> Italy                                                       1       0     4
#> Jamaica                                                     0       0     0
#> Japan                                                       2       0     0
#> Jordan                                                      0       0    13
#> Kazakhstan                                                  0       0     0
#> Kenya                                                       0       0     2
#> Korea, Democratic People's Republic Of                      0       0     0
#> Korea, Republic Of                                          0       0     0
#> Kuwait                                                      0       0    20
#> Kyrgyzstan                                                  0       0     0
#> Lao People's Democratic Republic                            0       0     0
#> Latvia                                                      0       0     0
#> Lebanon                                                     0       0     9
#> Liberia                                                     0       0     0
#> Libyan Arab Jamahiriya                                      0       0     2
#> Lithuania                                                   0       0     0
#> Luxembourg                                                  0       0     0
#> Macedonia, The Former Yugoslav Republic Of                  0       0     0
#> Madagascar                                                  0       0     0
#> Malawi                                                      0       0     0
#> Malaysia                                                    0       0    10
#> Mali                                                        0       0     0
#> Mauritania                                                  0       0     0
#> Mauritius                                                   0       0     0
#> Mexico                                                      1       6     0
#> Moldova, Republic Of                                        0       0     2
#> Mongolia                                                    0       0     0
#> Morocco                                                     0       0     0
#> Mozambique                                                  0       0     0
#> Myanmar                                                     0       0     0
#> Nepal                                                       0       0     0
#> Netherlands                                                 0       0     1
#> New Zealand                                                 0       0     0
#> Nicaragua                                                   0       0     0
#> Niger                                                       0       0     0
#> Nigeria                                                     0       1     1
#> Norway                                                      0       0     0
#> Oman                                                        0       0     0
#> Pakistan                                                    1       0     0
#> Panama                                                      1       0     0
#> Papua New Guinea                                            0       0     0
#> Paraguay                                                    0       1     0
#> Peru                                                        0       2     1
#> Philippines                                                 0       0     0
#> Poland                                                      0       0     0
#> Portugal                                                    0       0     0
#> Qatar                                                       0       0     3
#> Romania                                                     0       0     2
#> Russian Federation                                          0       1     1
#> Rwanda                                                      0       0     0
#> Saudi Arabia                                                0       0     3
#> Senegal                                                     0       0     1
#> Sierra Leone                                                0       0     0
#> Singapore                                                   0       0     0
#> Slovakia                                                    0       0     1
#> Slovenia                                                    0       0     0
#> Solomon Islands                                             0       0     0
#> Somalia                                                     0       0     5
#> South Africa                                                0       0     0
#> Spain                                                       0       3     0
#> Sri Lanka                                                   0       0     0
#> Sudan                                                       0       0     3
#> Suriname                                                    0       0    10
#> Sweden                                                      0       0     1
#> Switzerland                                                 0       0     0
#> Syrian Arab Republic                                        0       0     4
#> Tajikistan                                                  0       0     0
#> Tanzania, United Republic Of                                0       0     0
#> Thailand                                                    0       0     0
#> Togo                                                        0       0     0
#> Trinidad And Tobago                                         0       0     0
#> Tunisia                                                     0       0     4
#> Turkmenistan                                                0       0     0
#> Uganda                                                      0       0     0
#> Ukraine                                                     0       0     0
#> United Arab Emirates                                        0       0     1
#> United Kingdom                                              0       0     2
#> United States                                               5      14    17
#> Uruguay                                                     0       0     0
#> Uzbekistan                                                  0       0     1
#> Vietnam                                                     0       0     0
#> Yemen                                                       0       0     7
#> Zambia                                                      0       0     0
#> Zimbabwe                                                    0       4     0
#>                                            El Salvador Equatorial Guinea
#> Afghanistan                                          0                 0
#> Albania                                              0                 0
#> Algeria                                              0                 0
#> Angola                                               0                 0
#> Argentina                                            0                 0
#> Armenia                                              0                 0
#> Australia                                            0                 0
#> Austria                                              0                 0
#> Azerbaijan                                           0                 0
#> Bahrain                                              0                 0
#> Bangladesh                                           0                 0
#> Belarus                                              0                 0
#> Belgium                                              0                 0
#> Benin                                                0                 0
#> Bolivia, Plurinational State Of                      0                 0
#> Bosnia And Herzegovina                               0                 0
#> Brazil                                               1                 0
#> Bulgaria                                             0                 0
#> Burkina Faso                                         0                 0
#> Burundi                                              0                 0
#> Cambodia                                             0                 0
#> Cameroon                                             0                 1
#> Canada                                               0                 0
#> Cape Verde                                           0                 0
#> Central African Republic                             0                 0
#> Chad                                                 0                 0
#> Chile                                                0                 0
#> China                                                0                 0
#> Colombia                                             0                 0
#> Comoros                                              0                 0
#> Congo, Republic Of                                   0                 0
#> Congo, The Democratic Republic Of                    0                 0
#> Costa Rica                                           0                 0
#> Cote D'ivoire                                        0                 0
#> Croatia                                              0                 0
#> Cuba                                                 2                 0
#> Cyprus                                               0                 0
#> Denmark                                              0                 0
#> Djibouti                                             0                 0
#> Dominican Republic                                   1                 0
#> Ecuador                                              1                 0
#> Egypt                                                0                 0
#> El Salvador                                         NA                 0
#> Equatorial Guinea                                    0                NA
#> Estonia                                              0                 0
#> Ethiopia                                             0                 0
#> Fiji                                                 0                 0
#> Finland                                              0                 0
#> France                                               0                 0
#> Gabon                                                0                 0
#> Gambia                                               0                 0
#> Georgia                                              0                 0
#> Germany                                              0                 0
#> Ghana                                                0                 0
#> Greece                                               0                 0
#> Guatemala                                            8                 0
#> Guinea                                               0                 0
#> Guinea-Bissau                                        0                 0
#> Guyana                                               0                 0
#> Haiti                                                0                 0
#> Honduras                                             2                 0
#> Hungary                                              0                 0
#> India                                                0                 0
#> Indonesia                                            0                 0
#> Iran, Islamic Republic Of                            0                 0
#> Iraq                                                 0                 0
#> Ireland                                              0                 0
#> Israel                                               0                 0
#> Italy                                                1                 0
#> Jamaica                                              0                 0
#> Japan                                                0                 0
#> Jordan                                               0                 0
#> Kazakhstan                                           0                 0
#> Kenya                                                0                 0
#> Korea, Democratic People's Republic Of               0                 0
#> Korea, Republic Of                                   0                 0
#> Kuwait                                               0                 0
#> Kyrgyzstan                                           0                 0
#> Lao People's Democratic Republic                     0                 0
#> Latvia                                               0                 0
#> Lebanon                                              0                 0
#> Liberia                                              0                 0
#> Libyan Arab Jamahiriya                               0                 0
#> Lithuania                                            0                 0
#> Luxembourg                                           0                 0
#> Macedonia, The Former Yugoslav Republic Of           0                 0
#> Madagascar                                           0                 0
#> Malawi                                               0                 0
#> Malaysia                                             0                 0
#> Mali                                                 0                 0
#> Mauritania                                           0                 0
#> Mauritius                                            0                 0
#> Mexico                                               8                 0
#> Moldova, Republic Of                                 0                 0
#> Mongolia                                             0                 0
#> Morocco                                              0                 0
#> Mozambique                                           0                 0
#> Myanmar                                              0                 0
#> Nepal                                                0                 0
#> Netherlands                                          0                 0
#> New Zealand                                          0                 0
#> Nicaragua                                            2                 0
#> Niger                                                0                 0
#> Nigeria                                              0                 1
#> Norway                                               0                 0
#> Oman                                                 0                 0
#> Pakistan                                             0                 0
#> Panama                                               0                 0
#> Papua New Guinea                                     0                 0
#> Paraguay                                             0                 0
#> Peru                                                 0                 0
#> Philippines                                          0                 0
#> Poland                                               0                 0
#> Portugal                                             0                 0
#> Qatar                                                0                 0
#> Romania                                              0                 0
#> Russian Federation                                   0                 0
#> Rwanda                                               0                 0
#> Saudi Arabia                                         0                 0
#> Senegal                                              0                 0
#> Sierra Leone                                         0                 0
#> Singapore                                            0                 0
#> Slovakia                                             0                 0
#> Slovenia                                             0                 0
#> Solomon Islands                                      0                 0
#> Somalia                                              0                 0
#> South Africa                                         0                 0
#> Spain                                                2                 0
#> Sri Lanka                                            0                 0
#> Sudan                                                0                 0
#> Suriname                                             0                 0
#> Sweden                                               0                 0
#> Switzerland                                          0                 0
#> Syrian Arab Republic                                 0                 0
#> Tajikistan                                           0                 0
#> Tanzania, United Republic Of                         0                 0
#> Thailand                                             0                 0
#> Togo                                                 0                 0
#> Trinidad And Tobago                                  0                 0
#> Tunisia                                              0                 0
#> Turkmenistan                                         0                 0
#> Uganda                                               0                 0
#> Ukraine                                              0                 0
#> United Arab Emirates                                 0                 0
#> United Kingdom                                       0                 0
#> United States                                       11                 0
#> Uruguay                                              0                 0
#> Uzbekistan                                           0                 0
#> Vietnam                                              0                 0
#> Yemen                                                0                 0
#> Zambia                                               0                 0
#> Zimbabwe                                             0                 0
#>                                            Estonia Ethiopia Fiji Finland France
#> Afghanistan                                      6        1    0       4     48
#> Albania                                          0        0    0       0      0
#> Algeria                                          0        0    0       0     11
#> Angola                                           0        0    0       0      2
#> Argentina                                        0        0    0       0      1
#> Armenia                                          0        0    0       0      0
#> Australia                                        0        0   25       0      4
#> Austria                                          0        0    0       0      0
#> Azerbaijan                                       0        0    0       0      1
#> Bahrain                                          0        0    0       0      0
#> Bangladesh                                       0        0    0       0      0
#> Belarus                                          0        0    0       0      1
#> Belgium                                          1        0    0       0      1
#> Benin                                            0        0    0       0      0
#> Bolivia, Plurinational State Of                  0        0    0       0      1
#> Bosnia And Herzegovina                           0        0    0       0      0
#> Brazil                                           0        0    0       0      2
#> Bulgaria                                         0        0    0       0      5
#> Burkina Faso                                     0        0    0       0      6
#> Burundi                                          0        0    0       0      0
#> Cambodia                                         0        0    0       0      3
#> Cameroon                                         0        0    0       0      1
#> Canada                                           0        1    0       2      2
#> Cape Verde                                       0        0    0       0      1
#> Central African Republic                         0        0    0       0      0
#> Chad                                             0        0    0       1      0
#> Chile                                            0        0    0       0      1
#> China                                            0        2    0       0      3
#> Colombia                                         0        0    0       0      1
#> Comoros                                          0        0    0       0      0
#> Congo, Republic Of                               0        0    0       0      2
#> Congo, The Democratic Republic Of                0        0    0       0      2
#> Costa Rica                                       0        0    0       0      0
#> Cote D'ivoire                                    0        0    0       0      6
#> Croatia                                          0        0    0       0      0
#> Cuba                                             0        0    0       0      0
#> Cyprus                                           0        0    0       0      0
#> Denmark                                          0        0    0       0      0
#> Djibouti                                         0        1    0       0      0
#> Dominican Republic                               0        0    0       0      1
#> Ecuador                                          0        0    0       0      1
#> Egypt                                            0        2    0       0      6
#> El Salvador                                      0        0    0       0      0
#> Equatorial Guinea                                0        0    0       0      0
#> Estonia                                         NA        0    0       1      2
#> Ethiopia                                         0       NA    0       0      6
#> Fiji                                             0        0   NA       0      0
#> Finland                                          1        0    0      NA      0
#> France                                           2        6    0       0     NA
#> Gabon                                            0        0    0       0      0
#> Gambia                                           2        0    0       0      0
#> Georgia                                          1        0    0       0      1
#> Germany                                          1        0    0       0     18
#> Ghana                                            0        0    0       0      1
#> Greece                                           0        0    0       0      2
#> Guatemala                                        0        0    0       0      0
#> Guinea                                           0        0    0       0      1
#> Guinea-Bissau                                    0        0    0       0      1
#> Guyana                                           0        0    0       0      0
#> Haiti                                            0        0    0       0      1
#> Honduras                                         0        0    0       0      0
#> Hungary                                          0        0    0       0     13
#> India                                            0        3    0       0      7
#> Indonesia                                        0        0    0       0      8
#> Iran, Islamic Republic Of                        0        0    0       0     16
#> Iraq                                             0        0    1       4      2
#> Ireland                                          0        0    0       0      1
#> Israel                                           0        4    0       2     22
#> Italy                                            0        0    0       0      4
#> Jamaica                                          0        0    0       0      0
#> Japan                                            0        0    0       0      0
#> Jordan                                           0        0    0       0      0
#> Kazakhstan                                       0        0    0       0      0
#> Kenya                                            0       13    0       0      2
#> Korea, Democratic People's Republic Of           0        0    0       0      0
#> Korea, Republic Of                               0        0    0       0      1
#> Kuwait                                           0        0    0       0      0
#> Kyrgyzstan                                       0        0    0       0      0
#> Lao People's Democratic Republic                 0        0    0       0      0
#> Latvia                                           1        0    0       3      0
#> Lebanon                                          0        0    0       0      1
#> Liberia                                          0        0    0       0      1
#> Libyan Arab Jamahiriya                           0        4    0       0      3
#> Lithuania                                        2        0    0       0      1
#> Luxembourg                                       0        0    0       0      1
#> Macedonia, The Former Yugoslav Republic Of       0        0    0       0      2
#> Madagascar                                       0        0    0       0      0
#> Malawi                                           0        0    0       0      0
#> Malaysia                                         0        0    0       0      1
#> Mali                                             0        0    0       0      3
#> Mauritania                                       0        0    0       0     14
#> Mauritius                                        0        0    0       0      0
#> Mexico                                           0        0    0       0      1
#> Moldova, Republic Of                             0        0    0       0      0
#> Mongolia                                         0        0    0       0      0
#> Morocco                                          0        0    0       0      4
#> Mozambique                                       0        0    0       1      0
#> Myanmar                                          0        0    0       0      2
#> Nepal                                            0        0    0       0      1
#> Netherlands                                      0        0    0       0      3
#> New Zealand                                      0        0    3       0      0
#> Nicaragua                                        0        0    0       0      0
#> Niger                                            0        0    0       0     17
#> Nigeria                                          0        2    0       0      7
#> Norway                                           0        0    0       0      0
#> Oman                                             0        0    0       0      0
#> Pakistan                                         0        0    0       0      6
#> Panama                                           0        0    0       0     82
#> Papua New Guinea                                 0        0    0       0      0
#> Paraguay                                         0        0    0       0      0
#> Peru                                             0        0    0       0      0
#> Philippines                                      0        0    0       0      0
#> Poland                                           0        0    0       0      1
#> Portugal                                         0        0    0       0      1
#> Qatar                                            0        0    0       0      0
#> Romania                                          0        0    0       0      1
#> Russian Federation                               6        0    0       3      9
#> Rwanda                                           0        3    0       6      8
#> Saudi Arabia                                     0        2    0       0      5
#> Senegal                                          0        0    0       0      0
#> Sierra Leone                                     0        0    0       0      0
#> Singapore                                        0        0    0       0      0
#> Slovakia                                         0        0    0       0      0
#> Slovenia                                         0        0    0       0      0
#> Solomon Islands                                  0        0    0       0      0
#> Somalia                                          0       41    0       0      1
#> South Africa                                     0        3    0       0      0
#> Spain                                            0        0    0       0     36
#> Sri Lanka                                        0        0    0       0      2
#> Sudan                                            0        1    0       0      2
#> Suriname                                         0        0    0       0      0
#> Sweden                                           0        2    0       0      0
#> Switzerland                                      0        0    0       0      1
#> Syrian Arab Republic                             0        0    0       0      1
#> Tajikistan                                       0        0    0       0      0
#> Tanzania, United Republic Of                     0        2    0       0      0
#> Thailand                                         0        1    0       0      4
#> Togo                                             0        0    0       0      0
#> Trinidad And Tobago                              0        0    0       0      0
#> Tunisia                                          0        0    0       0      2
#> Turkmenistan                                     0        0    0       0      0
#> Uganda                                           0        4    0       1      0
#> Ukraine                                          0        0    0       2      4
#> United Arab Emirates                             0        0    0       0      3
#> United Kingdom                                   0        2    0       0      6
#> United States                                    2        2    1       2     10
#> Uruguay                                          0        0    0       0      1
#> Uzbekistan                                       0        0    0       0      1
#> Vietnam                                          0        0    0       0      2
#> Yemen                                            0       75    0       0     14
#> Zambia                                           0        0    0       0      0
#> Zimbabwe                                         0        1    0       0      0
#>                                            Gabon Gambia Georgia Germany Ghana
#> Afghanistan                                    0      0      14      80     0
#> Albania                                        0      0       0       2     0
#> Algeria                                        0      0       0       1     0
#> Angola                                         0      0       0       0     1
#> Argentina                                      0      0       0       8     0
#> Armenia                                        0      0       5       1     0
#> Australia                                      0      0       0       1     0
#> Austria                                        0      0       3       2     0
#> Azerbaijan                                     0      0      11       1     0
#> Bahrain                                        0      0       0       0     0
#> Bangladesh                                     0      0       0       0     0
#> Belarus                                        0      0       4       4     0
#> Belgium                                        0      0       0       1     0
#> Benin                                          0      0       0       0     0
#> Bolivia, Plurinational State Of                0      0       0       1     0
#> Bosnia And Herzegovina                         0      0       0       0     0
#> Brazil                                         0      0       0       7     1
#> Bulgaria                                       0      0       0       2     0
#> Burkina Faso                                   0      0       0       0     0
#> Burundi                                        0      0       0       0     0
#> Cambodia                                       0      0       0       0     0
#> Cameroon                                       2      0       0       0     0
#> Canada                                         0      0       0       2     0
#> Cape Verde                                     0      0       0       0     0
#> Central African Republic                       0      0       0       0     0
#> Chad                                           0      0       0       0     0
#> Chile                                          0      0       0       0     0
#> China                                          1      0       0       1     5
#> Colombia                                       0      0       0       1     0
#> Comoros                                        0      0       0       0     0
#> Congo, Republic Of                             0      0       1       0     0
#> Congo, The Democratic Republic Of              0      0       0       0     0
#> Costa Rica                                     0      0       0       2     0
#> Cote D'ivoire                                  0      0       0       0     0
#> Croatia                                        0      0       0       0     0
#> Cuba                                           0      0       0       0     0
#> Cyprus                                         0      0       0       0     0
#> Denmark                                        0      0       0       1     0
#> Djibouti                                       0      0       0       1     0
#> Dominican Republic                             0      0       0       0     0
#> Ecuador                                        0      0       0       0     0
#> Egypt                                          0      0       0       1     0
#> El Salvador                                    0      0       0       0     0
#> Equatorial Guinea                              0      0       0       0     0
#> Estonia                                        0      2       1       1     0
#> Ethiopia                                       0      0       0       0     0
#> Fiji                                           0      0       0       0     0
#> Finland                                        0      0       0       0     0
#> France                                         0      0       1      18     1
#> Gabon                                         NA      0       0       0     0
#> Gambia                                         0     NA       0       0     0
#> Georgia                                        0      0      NA       1     0
#> Germany                                        0      0       1      NA     1
#> Ghana                                          0      0       0       1    NA
#> Greece                                         0      0       6       2     0
#> Guatemala                                      0      0       0       0     0
#> Guinea                                         0      0       0       0     0
#> Guinea-Bissau                                  0      0       0       0     0
#> Guyana                                         0      0       0       0     0
#> Haiti                                          0      0       0       0     0
#> Honduras                                       0      0       0       1     0
#> Hungary                                        0      0       0       1     0
#> India                                          0      0       0       3     0
#> Indonesia                                      0      0       0       0     0
#> Iran, Islamic Republic Of                      0     29       2      67     0
#> Iraq                                           0      0       0       4     0
#> Ireland                                        0      0       2       0     0
#> Israel                                         0      0       3      16     0
#> Italy                                          0      0       1      10     0
#> Jamaica                                        0      0       0       0     0
#> Japan                                          0      0       0       0     2
#> Jordan                                         0      0       0       1     0
#> Kazakhstan                                     0      0       0       1     0
#> Kenya                                          0      1       0       9     0
#> Korea, Democratic People's Republic Of         0      0       0       1     0
#> Korea, Republic Of                             0      0       0       1     0
#> Kuwait                                         0      0       0       0     0
#> Kyrgyzstan                                     0      0       0       0     0
#> Lao People's Democratic Republic               0      0       0       0     0
#> Latvia                                         0      0       0       1     0
#> Lebanon                                        0      0       0       0     0
#> Liberia                                        0      0       0       0     2
#> Libyan Arab Jamahiriya                         0      0       0       5     2
#> Lithuania                                      0      0       1       5     0
#> Luxembourg                                     0      0       0       0     0
#> Macedonia, The Former Yugoslav Republic Of     0      0       0       0     0
#> Madagascar                                     0      0       0       0     0
#> Malawi                                         0      0       0       0     0
#> Malaysia                                       0      1      14       0     0
#> Mali                                           0      0       0       0     0
#> Mauritania                                     0      1       0       0     0
#> Mauritius                                      0      0       0       0     0
#> Mexico                                         0      0       2       2     0
#> Moldova, Republic Of                           0      0       0       0     0
#> Mongolia                                       0      0       0       0     0
#> Morocco                                        0      0       0       2     0
#> Mozambique                                     0      0       0       0     0
#> Myanmar                                        0      0       0       0     0
#> Nepal                                          0      0       0       2     0
#> Netherlands                                    0      0       0       4     0
#> New Zealand                                    0      0       0       3     0
#> Nicaragua                                      0      0       1       0     0
#> Niger                                          0      0       0       1     1
#> Nigeria                                        0      3       0      10    13
#> Norway                                         0      0       0       0     0
#> Oman                                           0      0       0       0     0
#> Pakistan                                       0      0       1      19     0
#> Panama                                         0      0       0       0     0
#> Papua New Guinea                               0      0       0       0     0
#> Paraguay                                       0      0       0       1     0
#> Peru                                           0      0       0       0     0
#> Philippines                                    0      0       3       4     0
#> Poland                                         0      0      11       3     0
#> Portugal                                       0      0       0       0     0
#> Qatar                                          0      0       0       0     0
#> Romania                                        0      0       0       1     0
#> Russian Federation                             0      0     172      10     1
#> Rwanda                                         2      0       0       7     0
#> Saudi Arabia                                   0      0       0       0     0
#> Senegal                                        0      0       0       0     0
#> Sierra Leone                                   0      0       0       0     0
#> Singapore                                      0      0       0       0     0
#> Slovakia                                       0      0       2       2     0
#> Slovenia                                       0      0       0       0     0
#> Solomon Islands                                0      0       0       0     0
#> Somalia                                        0      0       6       7     4
#> South Africa                                   0      0       0       3     0
#> Spain                                          0      3      10       1     0
#> Sri Lanka                                      0      0       0       3     0
#> Sudan                                          0      0       0      11     0
#> Suriname                                       0      0       0       0     0
#> Sweden                                         0      0       0       2     0
#> Switzerland                                    0      0       1       4     1
#> Syrian Arab Republic                           0      0       0       3     0
#> Tajikistan                                     0      0       0       0     0
#> Tanzania, United Republic Of                   0      0       0       0     0
#> Thailand                                       0      0       0       3     1
#> Togo                                           0      0       0       0     0
#> Trinidad And Tobago                            0      0       0       0     0
#> Tunisia                                        0      0       0       0     0
#> Turkmenistan                                   0      0       0       0     0
#> Uganda                                         0      0       0       0     0
#> Ukraine                                        0      0      14       4     0
#> United Arab Emirates                           0      0       0       1     0
#> United Kingdom                                 0      0       1       6     1
#> United States                                  0      2      23      71     0
#> Uruguay                                        0      0       0       0     0
#> Uzbekistan                                     0      0       0       0     0
#> Vietnam                                        0      0       0       5     0
#> Yemen                                          0      0       0       6     0
#> Zambia                                         0      0       0       0     0
#> Zimbabwe                                       0      0       0       2     0
#>                                            Greece Guatemala Guinea
#> Afghanistan                                     4         0      0
#> Albania                                        36         0      0
#> Algeria                                         2         0      0
#> Angola                                          1         0      0
#> Argentina                                       2         0      0
#> Armenia                                         0         0      0
#> Australia                                       0         0      0
#> Austria                                         4         0      0
#> Azerbaijan                                      0         0      0
#> Bahrain                                         0         0      0
#> Bangladesh                                      1         0      0
#> Belarus                                         0         0      0
#> Belgium                                         1         0      0
#> Benin                                           0         0      0
#> Bolivia, Plurinational State Of                 0         0      0
#> Bosnia And Herzegovina                          0         0      0
#> Brazil                                          0         1      0
#> Bulgaria                                       10         0      0
#> Burkina Faso                                    1         0      0
#> Burundi                                         0         0      0
#> Cambodia                                        0         0      0
#> Cameroon                                        0         0      0
#> Canada                                          0         0      0
#> Cape Verde                                      0         0      1
#> Central African Republic                        0         0      0
#> Chad                                            0         0      1
#> Chile                                           0         0      0
#> China                                           0         0      3
#> Colombia                                        0         2      0
#> Comoros                                         0         0      0
#> Congo, Republic Of                              0         0      2
#> Congo, The Democratic Republic Of               0         0      0
#> Costa Rica                                      0         1      0
#> Cote D'ivoire                                   0         0      0
#> Croatia                                         0         0      0
#> Cuba                                            0         1      0
#> Cyprus                                          1         0      0
#> Denmark                                         0         0      0
#> Djibouti                                        1         0      0
#> Dominican Republic                              0         0      0
#> Ecuador                                         0         2      0
#> Egypt                                           1         0      0
#> El Salvador                                     0         8      0
#> Equatorial Guinea                               0         0      0
#> Estonia                                         0         0      0
#> Ethiopia                                        0         0      0
#> Fiji                                            0         0      0
#> Finland                                         0         0      0
#> France                                          2         0      1
#> Gabon                                           0         0      0
#> Gambia                                          0         0      0
#> Georgia                                         6         0      0
#> Germany                                         2         0      0
#> Ghana                                           0         0      0
#> Greece                                         NA         0      0
#> Guatemala                                       0        NA      0
#> Guinea                                          0         0     NA
#> Guinea-Bissau                                   0         0      1
#> Guyana                                          0         0      0
#> Haiti                                           0         1      0
#> Honduras                                        0         2      0
#> Hungary                                         0         0      0
#> India                                           0         0      0
#> Indonesia                                       0         0      0
#> Iran, Islamic Republic Of                       2         0      0
#> Iraq                                            2         0      0
#> Ireland                                         0         0      0
#> Israel                                          4         0      0
#> Italy                                           0         0      0
#> Jamaica                                         0         0      0
#> Japan                                           0         0      0
#> Jordan                                          1         0      0
#> Kazakhstan                                      0         0      0
#> Kenya                                           0         0      0
#> Korea, Democratic People's Republic Of          0         0      0
#> Korea, Republic Of                              1         1      0
#> Kuwait                                          0         0      0
#> Kyrgyzstan                                      0         0      0
#> Lao People's Democratic Republic                0         0      0
#> Latvia                                          0         0      0
#> Lebanon                                         0         0      0
#> Liberia                                         0         0      2
#> Libyan Arab Jamahiriya                          3         0      0
#> Lithuania                                       0         0      0
#> Luxembourg                                      0         0      0
#> Macedonia, The Former Yugoslav Republic Of      1         0      0
#> Madagascar                                      0         0      0
#> Malawi                                          0         0      0
#> Malaysia                                        0         0      0
#> Mali                                            0         0      0
#> Mauritania                                      0         0      1
#> Mauritius                                       0         0      0
#> Mexico                                          0         9      0
#> Moldova, Republic Of                            0         0      0
#> Mongolia                                        0         0      0
#> Morocco                                         0         0      3
#> Mozambique                                      0         0      0
#> Myanmar                                         0         0      0
#> Nepal                                           0         0      0
#> Netherlands                                     0         0      0
#> New Zealand                                     0         0      0
#> Nicaragua                                       0         2      0
#> Niger                                           1         0      1
#> Nigeria                                         0         1      1
#> Norway                                          0         0      0
#> Oman                                            0         0      0
#> Pakistan                                        2         0      0
#> Panama                                          0         0      0
#> Papua New Guinea                                0         0      0
#> Paraguay                                        0         0      0
#> Peru                                            0         0      0
#> Philippines                                     0         0      0
#> Poland                                          2         0      0
#> Portugal                                        0         0      0
#> Qatar                                           0         0      0
#> Romania                                         8         0      0
#> Russian Federation                              5         0      0
#> Rwanda                                          0         0      0
#> Saudi Arabia                                    0         0      0
#> Senegal                                         0         0      1
#> Sierra Leone                                    0         0      1
#> Singapore                                       0         0      0
#> Slovakia                                        1         0      0
#> Slovenia                                        0         0      0
#> Solomon Islands                                 0         0      0
#> Somalia                                         2         0      3
#> South Africa                                    0         0      1
#> Spain                                           1        12      0
#> Sri Lanka                                       0         0      0
#> Sudan                                           2         0      0
#> Suriname                                        0         0      0
#> Sweden                                          0         0      0
#> Switzerland                                     0         0      1
#> Syrian Arab Republic                            1         0      0
#> Tajikistan                                      0         0      0
#> Tanzania, United Republic Of                    0         0      2
#> Thailand                                        0         1      2
#> Togo                                            0         0      2
#> Trinidad And Tobago                             0         0      0
#> Tunisia                                         0         0      0
#> Turkmenistan                                    0         0      0
#> Uganda                                          0         0      1
#> Ukraine                                         1         0      0
#> United Arab Emirates                            0         0      0
#> United Kingdom                                  1         0      0
#> United States                                   6        33      0
#> Uruguay                                         1         0      0
#> Uzbekistan                                      0         0      0
#> Vietnam                                         0         0      0
#> Yemen                                           0         0      0
#> Zambia                                          0         0      0
#> Zimbabwe                                        0         0      0
#>                                            Guinea-Bissau Guyana Haiti Honduras
#> Afghanistan                                            0      0     0        0
#> Albania                                                0      0     0        0
#> Algeria                                                0      0     0        0
#> Angola                                                 1      0     0        0
#> Argentina                                              0      0     1        2
#> Armenia                                                0      0     0        0
#> Australia                                              0      0     0        0
#> Austria                                                0      0     0        0
#> Azerbaijan                                             0      0     0        0
#> Bahrain                                                0      0     0        0
#> Bangladesh                                             0      0     0        0
#> Belarus                                                0      0     0        0
#> Belgium                                                0      0     0        0
#> Benin                                                  0      0     0        0
#> Bolivia, Plurinational State Of                        0      0     0        1
#> Bosnia And Herzegovina                                 0      0     0        0
#> Brazil                                                 0      0     1        3
#> Bulgaria                                               0      0     0        0
#> Burkina Faso                                           0      0     0        0
#> Burundi                                                0      0     0        0
#> Cambodia                                               0      0     0        0
#> Cameroon                                               0      0     0        0
#> Canada                                                 0      0     2        0
#> Cape Verde                                             0      0     0        0
#> Central African Republic                               0      0     0        0
#> Chad                                                   0      0     0        0
#> Chile                                                  0      0     0        1
#> China                                                  0      0     1        0
#> Colombia                                               0      0     0        1
#> Comoros                                                0      0     0        0
#> Congo, Republic Of                                     0      0     0        0
#> Congo, The Democratic Republic Of                      0      0     0        0
#> Costa Rica                                             0      0     0        0
#> Cote D'ivoire                                          0      0     1        0
#> Croatia                                                0      0     1        0
#> Cuba                                                   0      0     1        0
#> Cyprus                                                 0      0     0        0
#> Denmark                                                0      0     0        0
#> Djibouti                                               0      0     0        0
#> Dominican Republic                                     0      0     6        1
#> Ecuador                                                0      0     0        1
#> Egypt                                                  0      0     0        0
#> El Salvador                                            0      0     0        2
#> Equatorial Guinea                                      0      0     0        0
#> Estonia                                                0      0     0        0
#> Ethiopia                                               0      0     0        0
#> Fiji                                                   0      0     0        0
#> Finland                                                0      0     0        0
#> France                                                 1      0     1        0
#> Gabon                                                  0      0     0        0
#> Gambia                                                 0      0     0        0
#> Georgia                                                0      0     0        0
#> Germany                                                0      0     0        1
#> Ghana                                                  0      0     0        0
#> Greece                                                 0      0     0        0
#> Guatemala                                              0      0     1        2
#> Guinea                                                 1      0     0        0
#> Guinea-Bissau                                         NA      0     0        0
#> Guyana                                                 0     NA     0        0
#> Haiti                                                  0      0    NA        0
#> Honduras                                               0      0     0       NA
#> Hungary                                                0      0     0        0
#> India                                                  0      0     0        1
#> Indonesia                                              0      0     0        0
#> Iran, Islamic Republic Of                              0      0     0        0
#> Iraq                                                   0      0     0        0
#> Ireland                                                0      0     0        0
#> Israel                                                 0      0     0        0
#> Italy                                                  0      0     0        0
#> Jamaica                                                0      0     3        1
#> Japan                                                  0      0     4        0
#> Jordan                                                 0      0     1        0
#> Kazakhstan                                             0      0     0        0
#> Kenya                                                  0      0     0        0
#> Korea, Democratic People's Republic Of                 0      0     0        0
#> Korea, Republic Of                                     0      0     8        0
#> Kuwait                                                 0      0     0        0
#> Kyrgyzstan                                             0      0     0        0
#> Lao People's Democratic Republic                       0      0     0        0
#> Latvia                                                 0      0     0        0
#> Lebanon                                                0      0     0        0
#> Liberia                                                0      0     0        0
#> Libyan Arab Jamahiriya                                 0      0     0        0
#> Lithuania                                              0      0     0        0
#> Luxembourg                                             0      0     0        0
#> Macedonia, The Former Yugoslav Republic Of             0      0     0        0
#> Madagascar                                             0      0     0        0
#> Malawi                                                 0      0     0        0
#> Malaysia                                               0      0     0        0
#> Mali                                                   0      0     0        0
#> Mauritania                                             0      0     0        0
#> Mauritius                                              0      0     0        0
#> Mexico                                                 0      0     1       14
#> Moldova, Republic Of                                   0      0     0        0
#> Mongolia                                               0      0     0        0
#> Morocco                                                0      0     0        0
#> Mozambique                                             0      0     0        1
#> Myanmar                                                0      1     0        0
#> Nepal                                                  0      0     0        0
#> Netherlands                                            0      0     0        0
#> New Zealand                                            0      0     0        0
#> Nicaragua                                              0      0     0        8
#> Niger                                                  0      0     0        0
#> Nigeria                                                1      0     0        0
#> Norway                                                 0      0     0        0
#> Oman                                                   0      0     0        0
#> Pakistan                                               0      0     0        0
#> Panama                                                 0      0     0        1
#> Papua New Guinea                                       0      0     0        0
#> Paraguay                                               0      0     0        0
#> Peru                                                   0      0     1        0
#> Philippines                                            0      2     0        0
#> Poland                                                 0      0     0        0
#> Portugal                                               1      0     0        0
#> Qatar                                                  0      0     0        0
#> Romania                                                0      0     1        0
#> Russian Federation                                     0      4     2        0
#> Rwanda                                                 0      0     0        0
#> Saudi Arabia                                           0      0     0        0
#> Senegal                                                2      0     0        0
#> Sierra Leone                                           0      0     0        0
#> Singapore                                              0      0     0        0
#> Slovakia                                               0      0     0        0
#> Slovenia                                               0      0     0        0
#> Solomon Islands                                        0      0     0        0
#> Somalia                                                0      0     0        0
#> South Africa                                           0      0     0        0
#> Spain                                                  0      0     2        0
#> Sri Lanka                                              0      0     0        0
#> Sudan                                                  0      0     0        0
#> Suriname                                               0      0     0        0
#> Sweden                                                 0      0     2        0
#> Switzerland                                            0      0     0        0
#> Syrian Arab Republic                                   0      0     0        0
#> Tajikistan                                             0      0     0        0
#> Tanzania, United Republic Of                           0      0     0        0
#> Thailand                                               0      0     0        0
#> Togo                                                   0      0     2        0
#> Trinidad And Tobago                                    0      0     0        0
#> Tunisia                                                0      0     0        0
#> Turkmenistan                                           0      0     0        0
#> Uganda                                                 0      0     0        0
#> Ukraine                                                0      0     0        0
#> United Arab Emirates                                   0      0     0        0
#> United Kingdom                                         0      0     0        0
#> United States                                          1      1    63       27
#> Uruguay                                                0      0     0        0
#> Uzbekistan                                             0      0     0        0
#> Vietnam                                                0      0     0        0
#> Yemen                                                  0      0     0        0
#> Zambia                                                 0      0     0        0
#> Zimbabwe                                               0      0     0        0
#>                                            Hungary India Indonesia
#> Afghanistan                                      4    17        49
#> Albania                                          0     1         0
#> Algeria                                          0     0         1
#> Angola                                           0     0         0
#> Argentina                                        0     0         0
#> Armenia                                          0     0         0
#> Australia                                        3    21        43
#> Austria                                          3     5         0
#> Azerbaijan                                       0     5         0
#> Bahrain                                          0     1         0
#> Bangladesh                                       4    65         3
#> Belarus                                          0     1         0
#> Belgium                                          0     2         0
#> Benin                                            0     1         0
#> Bolivia, Plurinational State Of                  4     1         0
#> Bosnia And Herzegovina                           1     0         0
#> Brazil                                           0     2         0
#> Bulgaria                                         1     1         0
#> Burkina Faso                                     0     0         0
#> Burundi                                          0     0         0
#> Cambodia                                         0     0         0
#> Cameroon                                         0     0         0
#> Canada                                           0     2         1
#> Cape Verde                                       0     0         0
#> Central African Republic                         0     1         0
#> Chad                                             0     0         0
#> Chile                                            0     0         0
#> China                                            2    48         4
#> Colombia                                         0     5         1
#> Comoros                                          0     0         0
#> Congo, Republic Of                               0     7         0
#> Congo, The Democratic Republic Of                0     2         0
#> Costa Rica                                       0     0         0
#> Cote D'ivoire                                    0     0         0
#> Croatia                                          0     0         0
#> Cuba                                             0     0         0
#> Cyprus                                           0     0         0
#> Denmark                                          0     4         0
#> Djibouti                                         0     0         0
#> Dominican Republic                               0     1         0
#> Ecuador                                          0     4         0
#> Egypt                                            0     3         1
#> El Salvador                                      0     0         0
#> Equatorial Guinea                                0     0         0
#> Estonia                                          0     0         0
#> Ethiopia                                         0     3         0
#> Fiji                                             0     0         0
#> Finland                                          0     0         0
#> France                                          13     7         8
#> Gabon                                            0     0         0
#> Gambia                                           0     0         0
#> Georgia                                          0     0         0
#> Germany                                          1     3         0
#> Ghana                                            0     0         0
#> Greece                                           0     0         0
#> Guatemala                                        0     0         0
#> Guinea                                           0     0         0
#> Guinea-Bissau                                    0     0         0
#> Guyana                                           0     0         0
#> Haiti                                            0     0         0
#> Honduras                                         0     1         0
#> Hungary                                         NA     1         0
#> India                                            1    NA         4
#> Indonesia                                        0     4        NA
#> Iran, Islamic Republic Of                        0     6        17
#> Iraq                                             1     1         1
#> Ireland                                          0     0         0
#> Israel                                           2     2        14
#> Italy                                            4     1         2
#> Jamaica                                          0     0         0
#> Japan                                            0     7         8
#> Jordan                                           0     0         1
#> Kazakhstan                                       1     0         0
#> Kenya                                            0     0         0
#> Korea, Democratic People's Republic Of           0     0         2
#> Korea, Republic Of                               0     3         1
#> Kuwait                                           0     0         2
#> Kyrgyzstan                                       0     0         0
#> Lao People's Democratic Republic                 0     0         0
#> Latvia                                           0     1         1
#> Lebanon                                          0     0         0
#> Liberia                                          0     5         0
#> Libyan Arab Jamahiriya                           0     0         0
#> Lithuania                                        0     0         0
#> Luxembourg                                       0     0         0
#> Macedonia, The Former Yugoslav Republic Of       0     0         0
#> Madagascar                                       0     0         0
#> Malawi                                           0     0         0
#> Malaysia                                         0     9        68
#> Mali                                             0     0         0
#> Mauritania                                       0     0         0
#> Mauritius                                        0     0         0
#> Mexico                                           1     5         0
#> Moldova, Republic Of                             0     0         0
#> Mongolia                                         0     0         0
#> Morocco                                          0     0         0
#> Mozambique                                       0     0         0
#> Myanmar                                          0    19         1
#> Nepal                                            0    30         0
#> Netherlands                                      0     1         1
#> New Zealand                                      0     1         3
#> Nicaragua                                        0     0         0
#> Niger                                            0     0         1
#> Nigeria                                          0     4         5
#> Norway                                           0     1         2
#> Oman                                             0     0         0
#> Pakistan                                         0   208         1
#> Panama                                           0     0         0
#> Papua New Guinea                                 0     0         1
#> Paraguay                                         0     0         0
#> Peru                                             0     2         0
#> Philippines                                      0     4         6
#> Poland                                           2     1         0
#> Portugal                                         0     3         0
#> Qatar                                            0     1         0
#> Romania                                          5     0         0
#> Russian Federation                               5     8         1
#> Rwanda                                           0     1         0
#> Saudi Arabia                                     0     1        21
#> Senegal                                          1     0         0
#> Sierra Leone                                     0     0         0
#> Singapore                                        0     5         2
#> Slovakia                                        11     0         0
#> Slovenia                                         0     0         0
#> Solomon Islands                                  0     0         0
#> Somalia                                          0     5         1
#> South Africa                                     0     3         1
#> Spain                                            0     3         0
#> Sri Lanka                                        0    22         1
#> Sudan                                            0     4         1
#> Suriname                                         0     2         0
#> Sweden                                           0     0         0
#> Switzerland                                      0     0         1
#> Syrian Arab Republic                             0     0         2
#> Tajikistan                                       0     1         0
#> Tanzania, United Republic Of                     0     0         0
#> Thailand                                         0     3         2
#> Togo                                             0     0         0
#> Trinidad And Tobago                              0     0         0
#> Tunisia                                          0     0         0
#> Turkmenistan                                     0     0         0
#> Uganda                                           0     1         0
#> Ukraine                                          0     0         0
#> United Arab Emirates                             0    25         1
#> United Kingdom                                   0    17         2
#> United States                                    2    65       100
#> Uruguay                                          0     0         0
#> Uzbekistan                                       0     0         1
#> Vietnam                                          5     0         6
#> Yemen                                            0     4         0
#> Zambia                                           0     1         0
#> Zimbabwe                                         0     1         0
#>                                            Iran, Islamic Republic Of Iraq
#> Afghanistan                                                      141    4
#> Albania                                                            0    0
#> Algeria                                                            0    2
#> Angola                                                             0    0
#> Argentina                                                          1    0
#> Armenia                                                            4    0
#> Australia                                                          2    2
#> Austria                                                            1    0
#> Azerbaijan                                                        19    2
#> Bahrain                                                            2    0
#> Bangladesh                                                         0    0
#> Belarus                                                            4    0
#> Belgium                                                            4    2
#> Benin                                                              0    0
#> Bolivia, Plurinational State Of                                    0    0
#> Bosnia And Herzegovina                                             0    0
#> Brazil                                                             1    0
#> Bulgaria                                                           0    1
#> Burkina Faso                                                       0    0
#> Burundi                                                            0    0
#> Cambodia                                                           0    0
#> Cameroon                                                           0    1
#> Canada                                                             6    0
#> Cape Verde                                                         0    0
#> Central African Republic                                           0    0
#> Chad                                                               0    0
#> Chile                                                              0    0
#> China                                                              1    0
#> Colombia                                                           0    0
#> Comoros                                                            0    0
#> Congo, Republic Of                                                 0    0
#> Congo, The Democratic Republic Of                                  0    0
#> Costa Rica                                                         0    0
#> Cote D'ivoire                                                      0    0
#> Croatia                                                            0    3
#> Cuba                                                               0    0
#> Cyprus                                                             0    0
#> Denmark                                                            3    1
#> Djibouti                                                           0    0
#> Dominican Republic                                                 0    0
#> Ecuador                                                            0    0
#> Egypt                                                              6    9
#> El Salvador                                                        0    0
#> Equatorial Guinea                                                  0    0
#> Estonia                                                            0    0
#> Ethiopia                                                           0    0
#> Fiji                                                               0    1
#> Finland                                                            0    4
#> France                                                            16    2
#> Gabon                                                              0    0
#> Gambia                                                            29    0
#> Georgia                                                            2    0
#> Germany                                                           67    4
#> Ghana                                                              0    0
#> Greece                                                             2    2
#> Guatemala                                                          0    0
#> Guinea                                                             0    0
#> Guinea-Bissau                                                      0    0
#> Guyana                                                             0    0
#> Haiti                                                              0    0
#> Honduras                                                           0    0
#> Hungary                                                            0    1
#> India                                                              6    1
#> Indonesia                                                         17    1
#> Iran, Islamic Republic Of                                         NA   83
#> Iraq                                                              83   NA
#> Ireland                                                            0    0
#> Israel                                                           225    5
#> Italy                                                             33    0
#> Jamaica                                                            0    0
#> Japan                                                             26    0
#> Jordan                                                             0    2
#> Kazakhstan                                                         0    0
#> Kenya                                                              0    1
#> Korea, Democratic People's Republic Of                             2    0
#> Korea, Republic Of                                                30    1
#> Kuwait                                                             4    5
#> Kyrgyzstan                                                         2    0
#> Lao People's Democratic Republic                                   0    0
#> Latvia                                                             1    0
#> Lebanon                                                           24    1
#> Liberia                                                            0    0
#> Libyan Arab Jamahiriya                                             1    2
#> Lithuania                                                          0    0
#> Luxembourg                                                         0    0
#> Macedonia, The Former Yugoslav Republic Of                         0    0
#> Madagascar                                                         0    0
#> Malawi                                                             0    0
#> Malaysia                                                          36    2
#> Mali                                                               0    0
#> Mauritania                                                         0    0
#> Mauritius                                                          0    0
#> Mexico                                                             0    0
#> Moldova, Republic Of                                               0    0
#> Mongolia                                                           0    0
#> Morocco                                                            0    0
#> Mozambique                                                         0    0
#> Myanmar                                                            0    0
#> Nepal                                                              0    0
#> Netherlands                                                        1    0
#> New Zealand                                                        0    1
#> Nicaragua                                                          0    0
#> Niger                                                              0    0
#> Nigeria                                                           11    0
#> Norway                                                             5    2
#> Oman                                                               1    0
#> Pakistan                                                          25    1
#> Panama                                                             0    0
#> Papua New Guinea                                                   0    0
#> Paraguay                                                           0    0
#> Peru                                                               0    0
#> Philippines                                                        0    0
#> Poland                                                             0    5
#> Portugal                                                           0    0
#> Qatar                                                              3    1
#> Romania                                                            1    1
#> Russian Federation                                               108    4
#> Rwanda                                                             0    0
#> Saudi Arabia                                                       4    5
#> Senegal                                                           13    0
#> Sierra Leone                                                       0    0
#> Singapore                                                          0    0
#> Slovakia                                                           0    0
#> Slovenia                                                           0    0
#> Solomon Islands                                                    0    0
#> Somalia                                                           10    7
#> South Africa                                                       1    0
#> Spain                                                              1    1
#> Sri Lanka                                                          3    1
#> Sudan                                                              1    1
#> Suriname                                                           0    0
#> Sweden                                                             5    4
#> Switzerland                                                       11    0
#> Syrian Arab Republic                                               3    4
#> Tajikistan                                                         1    0
#> Tanzania, United Republic Of                                       0    0
#> Thailand                                                          46    1
#> Togo                                                               0    0
#> Trinidad And Tobago                                                0    3
#> Tunisia                                                            0    2
#> Turkmenistan                                                       1    0
#> Uganda                                                             0    0
#> Ukraine                                                            0    0
#> United Arab Emirates                                               7    7
#> United Kingdom                                                    29   41
#> United States                                                    683  260
#> Uruguay                                                            0    0
#> Uzbekistan                                                         0    0
#> Vietnam                                                            0    1
#> Yemen                                                              8    1
#> Zambia                                                             0    0
#> Zimbabwe                                                           2    0
#>                                            Ireland Israel Italy Jamaica Japan
#> Afghanistan                                      0      5    13       0     2
#> Albania                                          0      0     1       0     0
#> Algeria                                          0      6    13       0     0
#> Angola                                           0      0     0       0     0
#> Argentina                                        0      1     1       0     1
#> Armenia                                          0      2     0       0     0
#> Australia                                        2     21     2       0    11
#> Austria                                          0      1     0       0     0
#> Azerbaijan                                       0     13     1       0     0
#> Bahrain                                          0      1     0       0     0
#> Bangladesh                                       0      2     0       0     1
#> Belarus                                          0      0     0       0    26
#> Belgium                                          2      8     0       0     0
#> Benin                                            0      0     0       0     0
#> Bolivia, Plurinational State Of                  0      1     0       0     0
#> Bosnia And Herzegovina                           0      2     0       0     0
#> Brazil                                           0      2    11       0     0
#> Bulgaria                                         0      1     0       0     3
#> Burkina Faso                                     0      0     0       0     0
#> Burundi                                          0      0     0       0     0
#> Cambodia                                         0      0     0       0     3
#> Cameroon                                         0      0     0       0     1
#> Canada                                           0     18     1       0     0
#> Cape Verde                                       0      0     0       0     0
#> Central African Republic                         0      0     0       0     0
#> Chad                                             0      0     0       0     0
#> Chile                                            0      0     1       0     0
#> China                                            3      0     2       0   428
#> Colombia                                         0      4     0       0     3
#> Comoros                                          0      0     0       0     0
#> Congo, Republic Of                               0      0     0       0     0
#> Congo, The Democratic Republic Of                0      0     0       0     0
#> Costa Rica                                       0      1     0       0     0
#> Cote D'ivoire                                    0      0     1       0     1
#> Croatia                                          0      0     0       0     0
#> Cuba                                             0      3     0       1     0
#> Cyprus                                           0      2     0       0     0
#> Denmark                                          0      0     0       0     0
#> Djibouti                                         0      0     0       0     5
#> Dominican Republic                               0      0     1       0     2
#> Ecuador                                          0      0     0       0     0
#> Egypt                                            0     41     4       0     0
#> El Salvador                                      0      0     1       0     0
#> Equatorial Guinea                                0      0     0       0     0
#> Estonia                                          0      0     0       0     0
#> Ethiopia                                         0      4     0       0     0
#> Fiji                                             0      0     0       0     0
#> Finland                                          0      2     0       0     0
#> France                                           1     22     4       0     0
#> Gabon                                            0      0     0       0     0
#> Gambia                                           0      0     0       0     0
#> Georgia                                          2      3     1       0     0
#> Germany                                          0     16    10       0     0
#> Ghana                                            0      0     0       0     2
#> Greece                                           0      4     0       0     0
#> Guatemala                                        0      0     0       0     0
#> Guinea                                           0      0     0       0     0
#> Guinea-Bissau                                    0      0     0       0     0
#> Guyana                                           0      0     0       0     0
#> Haiti                                            0      0     0       3     4
#> Honduras                                         0      0     0       1     0
#> Hungary                                          0      2     4       0     0
#> India                                            0      2     1       0     7
#> Indonesia                                        0     14     2       0     8
#> Iran, Islamic Republic Of                        0    225    33       0    26
#> Iraq                                             0      5     0       0     0
#> Ireland                                         NA     23     1       0     2
#> Israel                                          23     NA     6       0     0
#> Italy                                            1      6    NA       0     2
#> Jamaica                                          0      0     0      NA     0
#> Japan                                            2      0     2       0    NA
#> Jordan                                           0     15     0       0     0
#> Kazakhstan                                       0      1     0       0     0
#> Kenya                                            0      0     1      23     0
#> Korea, Democratic People's Republic Of           0      0     0       0    89
#> Korea, Republic Of                               0      2     0       0    22
#> Kuwait                                           0      9     0       0     0
#> Kyrgyzstan                                       0      0     0       0     0
#> Lao People's Democratic Republic                 0      0     0       0     0
#> Latvia                                           1      0     0       0     0
#> Lebanon                                          2    529     0       0     0
#> Liberia                                          0      0     1       0     0
#> Libyan Arab Jamahiriya                           1      2     4       0     0
#> Lithuania                                        2      0     2       0     0
#> Luxembourg                                       0      0     0       0     0
#> Macedonia, The Former Yugoslav Republic Of       0      0     0       0     0
#> Madagascar                                       0      0     0       0     0
#> Malawi                                           0      0     0       0     0
#> Malaysia                                         0      7     0       0     0
#> Mali                                             0      0     2       0     0
#> Mauritania                                       0     30     1       0     0
#> Mauritius                                        0      0     0       0     0
#> Mexico                                           0      1     0       0     0
#> Moldova, Republic Of                             0      0     0       0     0
#> Mongolia                                         0      0     0       0     6
#> Morocco                                          0      7     2       0     0
#> Mozambique                                       0      0     0       0     0
#> Myanmar                                          0      1     0       0    14
#> Nepal                                            0      3     0       0     4
#> Netherlands                                      2      2     2       0     0
#> New Zealand                                      0      6     0       1     4
#> Nicaragua                                        0     33     0       0     0
#> Niger                                            0      0     0       0     0
#> Nigeria                                          0      0     2       3     2
#> Norway                                           0      2     0       0     0
#> Oman                                             0      0     0       0     0
#> Pakistan                                         0      5     1       0     1
#> Panama                                           0      0     0       0     0
#> Papua New Guinea                                 0      0     0       0     0
#> Paraguay                                         0      0     4       0     0
#> Peru                                             0      1     0       0     0
#> Philippines                                      0      3     0       0     9
#> Poland                                           1     49     0       0     1
#> Portugal                                         0      0     0       0     0
#> Qatar                                            0      3     0       0     0
#> Romania                                          0      2     0       0     0
#> Russian Federation                               0     11     1       0    16
#> Rwanda                                           0      0     0       0     0
#> Saudi Arabia                                     0      6     0       6     0
#> Senegal                                          0      3     0       0     0
#> Sierra Leone                                     0      0     0       0     0
#> Singapore                                        0      0     0       0     2
#> Slovakia                                         7      1     0       0     0
#> Slovenia                                         0      0     0       0     0
#> Solomon Islands                                  0      0     0       0     0
#> Somalia                                          0      0     1       0     1
#> South Africa                                     0     26     0       0     0
#> Spain                                            2      4     8       0     0
#> Sri Lanka                                        0      0     0       0     0
#> Sudan                                            0      4     0       0     0
#> Suriname                                         0      0     0       0     0
#> Sweden                                           1      1     0       0     0
#> Switzerland                                      0      1     1       0     0
#> Syrian Arab Republic                             0     10     0       0     3
#> Tajikistan                                       0      0     0       0     6
#> Tanzania, United Republic Of                     0      0     0       0     1
#> Thailand                                         0      3     0       0    21
#> Togo                                             0      0     0       0     0
#> Trinidad And Tobago                              0      0     0       0     0
#> Tunisia                                          0      3     0       0     0
#> Turkmenistan                                     0      0     0       0     0
#> Uganda                                           0      0     0       0     0
#> Ukraine                                          0      6     0       0     0
#> United Arab Emirates                             0     14     0       0     0
#> United Kingdom                                  20    122     2       4    11
#> United States                                    4     54    13      16   108
#> Uruguay                                          0      1     0       0     0
#> Uzbekistan                                       0      4     0       0     0
#> Vietnam                                          0      1     0       0     1
#> Yemen                                            0      1     0       0     2
#> Zambia                                           0      0     0       0     0
#> Zimbabwe                                         0      1     0       0     0
#>                                            Jordan Kazakhstan Kenya
#> Afghanistan                                    12          4     0
#> Albania                                         0          0     0
#> Algeria                                         0          0     0
#> Angola                                          0          0     0
#> Argentina                                       0          0     0
#> Armenia                                         0          0     0
#> Australia                                       1          0     4
#> Austria                                         0          0     0
#> Azerbaijan                                      0          3     0
#> Bahrain                                         0          0     0
#> Bangladesh                                      0          0     0
#> Belarus                                         0          0     0
#> Belgium                                         0          0     0
#> Benin                                           0          0     0
#> Bolivia, Plurinational State Of                 0          0     0
#> Bosnia And Herzegovina                          0          0     0
#> Brazil                                          1          0     0
#> Bulgaria                                        0          0     0
#> Burkina Faso                                    0          0     0
#> Burundi                                         0          0     1
#> Cambodia                                        0          0     0
#> Cameroon                                        0          0     0
#> Canada                                          0          0     0
#> Cape Verde                                      0          0     0
#> Central African Republic                        0          0     0
#> Chad                                            0          0     0
#> Chile                                           0          0     0
#> China                                           0          2     0
#> Colombia                                        0          0     0
#> Comoros                                         0          0     0
#> Congo, Republic Of                              0          0     0
#> Congo, The Democratic Republic Of               0          0     0
#> Costa Rica                                      0          0     0
#> Cote D'ivoire                                   0          0     0
#> Croatia                                         0          0     0
#> Cuba                                            0          0     0
#> Cyprus                                          0          0     0
#> Denmark                                         2          0     0
#> Djibouti                                        0          0     0
#> Dominican Republic                              0          0     0
#> Ecuador                                         0          0     0
#> Egypt                                          13          0     2
#> El Salvador                                     0          0     0
#> Equatorial Guinea                               0          0     0
#> Estonia                                         0          0     0
#> Ethiopia                                        0          0    13
#> Fiji                                            0          0     0
#> Finland                                         0          0     0
#> France                                          0          0     2
#> Gabon                                           0          0     0
#> Gambia                                          0          0     1
#> Georgia                                         0          0     0
#> Germany                                         1          1     9
#> Ghana                                           0          0     0
#> Greece                                          1          0     0
#> Guatemala                                       0          0     0
#> Guinea                                          0          0     0
#> Guinea-Bissau                                   0          0     0
#> Guyana                                          0          0     0
#> Haiti                                           1          0     0
#> Honduras                                        0          0     0
#> Hungary                                         0          1     0
#> India                                           0          0     0
#> Indonesia                                       1          0     0
#> Iran, Islamic Republic Of                       0          0     0
#> Iraq                                            2          0     1
#> Ireland                                         0          0     0
#> Israel                                         15          1     0
#> Italy                                           0          0     1
#> Jamaica                                         0          0    23
#> Japan                                           0          0     0
#> Jordan                                         NA          0     0
#> Kazakhstan                                      0         NA     0
#> Kenya                                           0          0    NA
#> Korea, Democratic People's Republic Of          0          0     0
#> Korea, Republic Of                              0          0     1
#> Kuwait                                          0          0     0
#> Kyrgyzstan                                      0          3     0
#> Lao People's Democratic Republic                0          0     0
#> Latvia                                          0          0     0
#> Lebanon                                         2          0     0
#> Liberia                                         0          0     0
#> Libyan Arab Jamahiriya                          0          0     0
#> Lithuania                                       0          0     0
#> Luxembourg                                      0          0     0
#> Macedonia, The Former Yugoslav Republic Of      0          0     0
#> Madagascar                                      0          0     0
#> Malawi                                          0          0     0
#> Malaysia                                        0          0     0
#> Mali                                            0          0     0
#> Mauritania                                      0          0     0
#> Mauritius                                       0          0     0
#> Mexico                                          0          0     0
#> Moldova, Republic Of                            0          0     0
#> Mongolia                                        0          0     0
#> Morocco                                         1          0     0
#> Mozambique                                      0          0     0
#> Myanmar                                         0          0     0
#> Nepal                                           0          0     0
#> Netherlands                                     0          0     1
#> New Zealand                                     0          0     0
#> Nicaragua                                       0          0     0
#> Niger                                           0          0     0
#> Nigeria                                         0          0     3
#> Norway                                          0          0     0
#> Oman                                            0          0     0
#> Pakistan                                        3          0     5
#> Panama                                          0          0     0
#> Papua New Guinea                                0          0     0
#> Paraguay                                        0          0     0
#> Peru                                            1          0     0
#> Philippines                                     0          0     0
#> Poland                                          0          0     0
#> Portugal                                        0          0     0
#> Qatar                                           0          0     0
#> Romania                                         0          0     0
#> Russian Federation                              1         13     2
#> Rwanda                                          0          0     0
#> Saudi Arabia                                    3          0    18
#> Senegal                                         0          0     0
#> Sierra Leone                                    0          0     0
#> Singapore                                       0          0     1
#> Slovakia                                        0          1     0
#> Slovenia                                        0          0     0
#> Solomon Islands                                 0          0     0
#> Somalia                                         0          0    35
#> South Africa                                    0          0     0
#> Spain                                           0          0     0
#> Sri Lanka                                       0          0     0
#> Sudan                                           3          0     2
#> Suriname                                        0          0     0
#> Sweden                                          0          0     0
#> Switzerland                                     0          0     0
#> Syrian Arab Republic                            0          0     0
#> Tajikistan                                      0          2     0
#> Tanzania, United Republic Of                    0          0     7
#> Thailand                                        0         10     3
#> Togo                                            0          0     0
#> Trinidad And Tobago                             0          0     2
#> Tunisia                                         0          0     0
#> Turkmenistan                                    0          2     0
#> Uganda                                          0          0    27
#> Ukraine                                         0          3     0
#> United Arab Emirates                            4          0     2
#> United Kingdom                                  2          0     4
#> United States                                  21          1    10
#> Uruguay                                         0          0     0
#> Uzbekistan                                      0          3     0
#> Vietnam                                         0          0     0
#> Yemen                                           3          0     0
#> Zambia                                          0          0     0
#> Zimbabwe                                        0          0     2
#>                                            Korea, Democratic People's Republic Of
#> Afghanistan                                                                     0
#> Albania                                                                         0
#> Algeria                                                                         0
#> Angola                                                                          0
#> Argentina                                                                       0
#> Armenia                                                                         0
#> Australia                                                                       3
#> Austria                                                                         0
#> Azerbaijan                                                                      0
#> Bahrain                                                                         2
#> Bangladesh                                                                      0
#> Belarus                                                                         0
#> Belgium                                                                         0
#> Benin                                                                           0
#> Bolivia, Plurinational State Of                                                 0
#> Bosnia And Herzegovina                                                          1
#> Brazil                                                                          0
#> Bulgaria                                                                        0
#> Burkina Faso                                                                    0
#> Burundi                                                                         0
#> Cambodia                                                                        0
#> Cameroon                                                                        0
#> Canada                                                                          3
#> Cape Verde                                                                      0
#> Central African Republic                                                        0
#> Chad                                                                            0
#> Chile                                                                           0
#> China                                                                          45
#> Colombia                                                                        0
#> Comoros                                                                         0
#> Congo, Republic Of                                                              0
#> Congo, The Democratic Republic Of                                               0
#> Costa Rica                                                                      0
#> Cote D'ivoire                                                                   0
#> Croatia                                                                         1
#> Cuba                                                                            0
#> Cyprus                                                                          0
#> Denmark                                                                         0
#> Djibouti                                                                        0
#> Dominican Republic                                                              0
#> Ecuador                                                                         0
#> Egypt                                                                           0
#> El Salvador                                                                     0
#> Equatorial Guinea                                                               0
#> Estonia                                                                         0
#> Ethiopia                                                                        0
#> Fiji                                                                            0
#> Finland                                                                         0
#> France                                                                          0
#> Gabon                                                                           0
#> Gambia                                                                          0
#> Georgia                                                                         0
#> Germany                                                                         1
#> Ghana                                                                           0
#> Greece                                                                          0
#> Guatemala                                                                       0
#> Guinea                                                                          0
#> Guinea-Bissau                                                                   0
#> Guyana                                                                          0
#> Haiti                                                                           0
#> Honduras                                                                        0
#> Hungary                                                                         0
#> India                                                                           0
#> Indonesia                                                                       2
#> Iran, Islamic Republic Of                                                       2
#> Iraq                                                                            0
#> Ireland                                                                         0
#> Israel                                                                          0
#> Italy                                                                           0
#> Jamaica                                                                         0
#> Japan                                                                          89
#> Jordan                                                                          0
#> Kazakhstan                                                                      0
#> Kenya                                                                           0
#> Korea, Democratic People's Republic Of                                         NA
#> Korea, Republic Of                                                           1845
#> Kuwait                                                                          0
#> Kyrgyzstan                                                                      0
#> Lao People's Democratic Republic                                                0
#> Latvia                                                                          0
#> Lebanon                                                                         0
#> Liberia                                                                         0
#> Libyan Arab Jamahiriya                                                          1
#> Lithuania                                                                       0
#> Luxembourg                                                                      0
#> Macedonia, The Former Yugoslav Republic Of                                      0
#> Madagascar                                                                      0
#> Malawi                                                                          0
#> Malaysia                                                                        0
#> Mali                                                                            0
#> Mauritania                                                                      0
#> Mauritius                                                                       0
#> Mexico                                                                          0
#> Moldova, Republic Of                                                            0
#> Mongolia                                                                        0
#> Morocco                                                                         0
#> Mozambique                                                                      0
#> Myanmar                                                                         5
#> Nepal                                                                           0
#> Netherlands                                                                     0
#> New Zealand                                                                     0
#> Nicaragua                                                                       0
#> Niger                                                                           0
#> Nigeria                                                                         0
#> Norway                                                                          0
#> Oman                                                                            0
#> Pakistan                                                                        0
#> Panama                                                                          0
#> Papua New Guinea                                                                0
#> Paraguay                                                                        0
#> Peru                                                                            0
#> Philippines                                                                     0
#> Poland                                                                          0
#> Portugal                                                                        1
#> Qatar                                                                           0
#> Romania                                                                         0
#> Russian Federation                                                             20
#> Rwanda                                                                          0
#> Saudi Arabia                                                                    0
#> Senegal                                                                         0
#> Sierra Leone                                                                    0
#> Singapore                                                                       0
#> Slovakia                                                                        0
#> Slovenia                                                                        0
#> Solomon Islands                                                                 0
#> Somalia                                                                         6
#> South Africa                                                                    0
#> Spain                                                                           0
#> Sri Lanka                                                                       1
#> Sudan                                                                           1
#> Suriname                                                                        0
#> Sweden                                                                          0
#> Switzerland                                                                     0
#> Syrian Arab Republic                                                            0
#> Tajikistan                                                                      0
#> Tanzania, United Republic Of                                                    0
#> Thailand                                                                        2
#> Togo                                                                            0
#> Trinidad And Tobago                                                             0
#> Tunisia                                                                         0
#> Turkmenistan                                                                    0
#> Uganda                                                                          0
#> Ukraine                                                                         1
#> United Arab Emirates                                                            0
#> United Kingdom                                                                  1
#> United States                                                                 165
#> Uruguay                                                                         0
#> Uzbekistan                                                                      0
#> Vietnam                                                                         0
#> Yemen                                                                           0
#> Zambia                                                                          0
#> Zimbabwe                                                                        0
#>                                            Korea, Republic Of Kuwait Kyrgyzstan
#> Afghanistan                                                 6      0         12
#> Albania                                                     0      0          0
#> Algeria                                                     0      0          0
#> Angola                                                      0      0          0
#> Argentina                                                   0      0          0
#> Armenia                                                     0      0          0
#> Australia                                                   3      0          0
#> Austria                                                     0      0          0
#> Azerbaijan                                                  3      0          0
#> Bahrain                                                     0      0          0
#> Bangladesh                                                  0      0          4
#> Belarus                                                     0      0          1
#> Belgium                                                     0      0          0
#> Benin                                                       0      0          0
#> Bolivia, Plurinational State Of                             0      0          0
#> Bosnia And Herzegovina                                      0      0          0
#> Brazil                                                      0      0          0
#> Bulgaria                                                    0      0          0
#> Burkina Faso                                                0      0          0
#> Burundi                                                     0      0          0
#> Cambodia                                                    1      0          0
#> Cameroon                                                    0      0          0
#> Canada                                                      0      0          0
#> Cape Verde                                                  0      0          0
#> Central African Republic                                    0      0          0
#> Chad                                                        0      0          0
#> Chile                                                       0      0          0
#> China                                                      23      0          4
#> Colombia                                                    0      0          0
#> Comoros                                                     0      0          0
#> Congo, Republic Of                                          0      0          0
#> Congo, The Democratic Republic Of                           0      0          0
#> Costa Rica                                                  1      0          0
#> Cote D'ivoire                                               0      0          0
#> Croatia                                                     0      0          0
#> Cuba                                                        1      1          0
#> Cyprus                                                      0      0          0
#> Denmark                                                     0      0          0
#> Djibouti                                                    0      0          0
#> Dominican Republic                                          0      0          0
#> Ecuador                                                     0      0          0
#> Egypt                                                       0     20          0
#> El Salvador                                                 0      0          0
#> Equatorial Guinea                                           0      0          0
#> Estonia                                                     0      0          0
#> Ethiopia                                                    0      0          0
#> Fiji                                                        0      0          0
#> Finland                                                     0      0          0
#> France                                                      1      0          0
#> Gabon                                                       0      0          0
#> Gambia                                                      0      0          0
#> Georgia                                                     0      0          0
#> Germany                                                     1      0          0
#> Ghana                                                       0      0          0
#> Greece                                                      1      0          0
#> Guatemala                                                   1      0          0
#> Guinea                                                      0      0          0
#> Guinea-Bissau                                               0      0          0
#> Guyana                                                      0      0          0
#> Haiti                                                       8      0          0
#> Honduras                                                    0      0          0
#> Hungary                                                     0      0          0
#> India                                                       3      0          0
#> Indonesia                                                   1      2          0
#> Iran, Islamic Republic Of                                  30      4          2
#> Iraq                                                        1      5          0
#> Ireland                                                     0      0          0
#> Israel                                                      2      9          0
#> Italy                                                       0      0          0
#> Jamaica                                                     0      0          0
#> Japan                                                      22      0          0
#> Jordan                                                      0      0          0
#> Kazakhstan                                                  0      0          3
#> Kenya                                                       1      0          0
#> Korea, Democratic People's Republic Of                   1845      0          0
#> Korea, Republic Of                                         NA      0          2
#> Kuwait                                                      0     NA          0
#> Kyrgyzstan                                                  2      0         NA
#> Lao People's Democratic Republic                            0      0          0
#> Latvia                                                      0      0          1
#> Lebanon                                                     1      2          0
#> Liberia                                                     0      0          0
#> Libyan Arab Jamahiriya                                     32      0          0
#> Lithuania                                                   0      0          1
#> Luxembourg                                                  0      0          0
#> Macedonia, The Former Yugoslav Republic Of                  0      0          0
#> Madagascar                                                  0      0          0
#> Malawi                                                      0      0          0
#> Malaysia                                                    1      0          0
#> Mali                                                        0      0          0
#> Mauritania                                                  0      0          0
#> Mauritius                                                   0      0          0
#> Mexico                                                      0      1          0
#> Moldova, Republic Of                                        0      0          0
#> Mongolia                                                    1      0          0
#> Morocco                                                     0      0          0
#> Mozambique                                                  0      0          0
#> Myanmar                                                     0      0          1
#> Nepal                                                      26      0          0
#> Netherlands                                                 0      0          0
#> New Zealand                                                 0      2          0
#> Nicaragua                                                   0      0          0
#> Niger                                                       0      0          0
#> Nigeria                                                     0      0          0
#> Norway                                                      0      0          0
#> Oman                                                        0      0          0
#> Pakistan                                                    4      0          2
#> Panama                                                      0      0          0
#> Papua New Guinea                                            0      0          0
#> Paraguay                                                    0      0          0
#> Peru                                                        0      0          0
#> Philippines                                                 6      0          0
#> Poland                                                      4      0          0
#> Portugal                                                    0      0          0
#> Qatar                                                       0      4          0
#> Romania                                                     0      0          0
#> Russian Federation                                         34      0         52
#> Rwanda                                                      0      0          0
#> Saudi Arabia                                                0      0          0
#> Senegal                                                     0      0          0
#> Sierra Leone                                                0      0          2
#> Singapore                                                   0      0          0
#> Slovakia                                                    0      0          0
#> Slovenia                                                    0      0          0
#> Solomon Islands                                             0      0          0
#> Somalia                                                     2      0          0
#> South Africa                                                0      0          0
#> Spain                                                       0      0          0
#> Sri Lanka                                                   6      0          0
#> Sudan                                                       0      0          0
#> Suriname                                                    0      0          0
#> Sweden                                                      0      0          0
#> Switzerland                                                 0      0          0
#> Syrian Arab Republic                                        0      0          0
#> Tajikistan                                                  0      0          3
#> Tanzania, United Republic Of                                0      0          0
#> Thailand                                                    1      0          0
#> Togo                                                        0      0          0
#> Trinidad And Tobago                                         0      0          0
#> Tunisia                                                     0      0          0
#> Turkmenistan                                                0      0          0
#> Uganda                                                      6      0          0
#> Ukraine                                                     0      0          0
#> United Arab Emirates                                        0      1          0
#> United Kingdom                                              0      1          4
#> United States                                              71      5          6
#> Uruguay                                                     1      0          0
#> Uzbekistan                                                 10      0        273
#> Vietnam                                                     1      0          0
#> Yemen                                                       0      0          0
#> Zambia                                                      0      0          0
#> Zimbabwe                                                    0      0          0
#>                                            Lao People's Democratic Republic
#> Afghanistan                                                               0
#> Albania                                                                   0
#> Algeria                                                                   0
#> Angola                                                                    0
#> Argentina                                                                 0
#> Armenia                                                                   0
#> Australia                                                                 0
#> Austria                                                                   0
#> Azerbaijan                                                                0
#> Bahrain                                                                   0
#> Bangladesh                                                                0
#> Belarus                                                                   0
#> Belgium                                                                   0
#> Benin                                                                     0
#> Bolivia, Plurinational State Of                                           0
#> Bosnia And Herzegovina                                                    0
#> Brazil                                                                    0
#> Bulgaria                                                                  0
#> Burkina Faso                                                              0
#> Burundi                                                                   0
#> Cambodia                                                                  1
#> Cameroon                                                                  0
#> Canada                                                                    0
#> Cape Verde                                                                0
#> Central African Republic                                                  0
#> Chad                                                                      0
#> Chile                                                                     0
#> China                                                                     0
#> Colombia                                                                  0
#> Comoros                                                                   0
#> Congo, Republic Of                                                        0
#> Congo, The Democratic Republic Of                                         0
#> Costa Rica                                                                0
#> Cote D'ivoire                                                             0
#> Croatia                                                                   0
#> Cuba                                                                      0
#> Cyprus                                                                    0
#> Denmark                                                                   0
#> Djibouti                                                                  0
#> Dominican Republic                                                        0
#> Ecuador                                                                   0
#> Egypt                                                                     0
#> El Salvador                                                               0
#> Equatorial Guinea                                                         0
#> Estonia                                                                   0
#> Ethiopia                                                                  0
#> Fiji                                                                      0
#> Finland                                                                   0
#> France                                                                    0
#> Gabon                                                                     0
#> Gambia                                                                    0
#> Georgia                                                                   0
#> Germany                                                                   0
#> Ghana                                                                     0
#> Greece                                                                    0
#> Guatemala                                                                 0
#> Guinea                                                                    0
#> Guinea-Bissau                                                             0
#> Guyana                                                                    0
#> Haiti                                                                     0
#> Honduras                                                                  0
#> Hungary                                                                   0
#> India                                                                     0
#> Indonesia                                                                 0
#> Iran, Islamic Republic Of                                                 0
#> Iraq                                                                      0
#> Ireland                                                                   0
#> Israel                                                                    0
#> Italy                                                                     0
#> Jamaica                                                                   0
#> Japan                                                                     0
#> Jordan                                                                    0
#> Kazakhstan                                                                0
#> Kenya                                                                     0
#> Korea, Democratic People's Republic Of                                    0
#> Korea, Republic Of                                                        0
#> Kuwait                                                                    0
#> Kyrgyzstan                                                                0
#> Lao People's Democratic Republic                                         NA
#> Latvia                                                                    0
#> Lebanon                                                                   0
#> Liberia                                                                   0
#> Libyan Arab Jamahiriya                                                    0
#> Lithuania                                                                 0
#> Luxembourg                                                                0
#> Macedonia, The Former Yugoslav Republic Of                                0
#> Madagascar                                                                0
#> Malawi                                                                    0
#> Malaysia                                                                  0
#> Mali                                                                      0
#> Mauritania                                                                0
#> Mauritius                                                                 0
#> Mexico                                                                    0
#> Moldova, Republic Of                                                      0
#> Mongolia                                                                  0
#> Morocco                                                                   0
#> Mozambique                                                                0
#> Myanmar                                                                   0
#> Nepal                                                                     0
#> Netherlands                                                               0
#> New Zealand                                                               0
#> Nicaragua                                                                 0
#> Niger                                                                     0
#> Nigeria                                                                   0
#> Norway                                                                    0
#> Oman                                                                      0
#> Pakistan                                                                  0
#> Panama                                                                    0
#> Papua New Guinea                                                          0
#> Paraguay                                                                  0
#> Peru                                                                      0
#> Philippines                                                               0
#> Poland                                                                    0
#> Portugal                                                                  0
#> Qatar                                                                     0
#> Romania                                                                   0
#> Russian Federation                                                        0
#> Rwanda                                                                    0
#> Saudi Arabia                                                              0
#> Senegal                                                                   0
#> Sierra Leone                                                              0
#> Singapore                                                                 0
#> Slovakia                                                                  0
#> Slovenia                                                                  0
#> Solomon Islands                                                           0
#> Somalia                                                                   0
#> South Africa                                                              0
#> Spain                                                                     0
#> Sri Lanka                                                                 0
#> Sudan                                                                     0
#> Suriname                                                                  0
#> Sweden                                                                    0
#> Switzerland                                                               0
#> Syrian Arab Republic                                                      0
#> Tajikistan                                                                0
#> Tanzania, United Republic Of                                              0
#> Thailand                                                                  2
#> Togo                                                                      0
#> Trinidad And Tobago                                                       0
#> Tunisia                                                                   0
#> Turkmenistan                                                              0
#> Uganda                                                                    0
#> Ukraine                                                                   0
#> United Arab Emirates                                                      0
#> United Kingdom                                                            0
#> United States                                                             1
#> Uruguay                                                                   0
#> Uzbekistan                                                                0
#> Vietnam                                                                   1
#> Yemen                                                                     0
#> Zambia                                                                    0
#> Zimbabwe                                                                  0
#>                                            Latvia Lebanon Liberia
#> Afghanistan                                     0       4       2
#> Albania                                         0       0       0
#> Algeria                                         0       0       0
#> Angola                                          0       0       0
#> Argentina                                       0       2       0
#> Armenia                                         0       2       0
#> Australia                                       0       1       0
#> Austria                                         0       0       0
#> Azerbaijan                                      1       2       0
#> Bahrain                                         0       0       0
#> Bangladesh                                      0       0       3
#> Belarus                                         0       0       0
#> Belgium                                         0       0       0
#> Benin                                           0       0       0
#> Bolivia, Plurinational State Of                 0       0       0
#> Bosnia And Herzegovina                          0       0       0
#> Brazil                                          0       1       0
#> Bulgaria                                        0       0       0
#> Burkina Faso                                    0       0       0
#> Burundi                                         0       0       0
#> Cambodia                                        0       1       0
#> Cameroon                                        0       0       0
#> Canada                                          0       0       0
#> Cape Verde                                      0       0       0
#> Central African Republic                        0       0       0
#> Chad                                            0       0       0
#> Chile                                           0       0       0
#> China                                           0       1       0
#> Colombia                                        0       1       0
#> Comoros                                         0       0       0
#> Congo, Republic Of                              0       0       0
#> Congo, The Democratic Republic Of               0       0       0
#> Costa Rica                                      0       0       0
#> Cote D'ivoire                                   0       0       1
#> Croatia                                         0       0       0
#> Cuba                                            0       0       0
#> Cyprus                                          0       0       0
#> Denmark                                         0       1       0
#> Djibouti                                        0       0       0
#> Dominican Republic                              0       0       0
#> Ecuador                                         0       0       0
#> Egypt                                           0       9       0
#> El Salvador                                     0       0       0
#> Equatorial Guinea                               0       0       0
#> Estonia                                         1       0       0
#> Ethiopia                                        0       0       0
#> Fiji                                            0       0       0
#> Finland                                         3       0       0
#> France                                          0       1       1
#> Gabon                                           0       0       0
#> Gambia                                          0       0       0
#> Georgia                                         0       0       0
#> Germany                                         1       0       0
#> Ghana                                           0       0       2
#> Greece                                          0       0       0
#> Guatemala                                       0       0       0
#> Guinea                                          0       0       2
#> Guinea-Bissau                                   0       0       0
#> Guyana                                          0       0       0
#> Haiti                                           0       0       0
#> Honduras                                        0       0       0
#> Hungary                                         0       0       0
#> India                                           1       0       5
#> Indonesia                                       1       0       0
#> Iran, Islamic Republic Of                       1      24       0
#> Iraq                                            0       1       0
#> Ireland                                         1       2       0
#> Israel                                          0     529       0
#> Italy                                           0       0       1
#> Jamaica                                         0       0       0
#> Japan                                           0       0       0
#> Jordan                                          0       2       0
#> Kazakhstan                                      0       0       0
#> Kenya                                           0       0       0
#> Korea, Democratic People's Republic Of          0       0       0
#> Korea, Republic Of                              0       1       0
#> Kuwait                                          0       2       0
#> Kyrgyzstan                                      1       0       0
#> Lao People's Democratic Republic                0       0       0
#> Latvia                                         NA       0       0
#> Lebanon                                         0      NA       0
#> Liberia                                         0       0      NA
#> Libyan Arab Jamahiriya                          0       5       0
#> Lithuania                                       1       1       0
#> Luxembourg                                      0       0       0
#> Macedonia, The Former Yugoslav Republic Of      0       0       0
#> Madagascar                                      0       0       0
#> Malawi                                          0       0       0
#> Malaysia                                        0       0       0
#> Mali                                            0       0       0
#> Mauritania                                      0       0       0
#> Mauritius                                       0       0       0
#> Mexico                                          0       3       0
#> Moldova, Republic Of                            0       0       0
#> Mongolia                                        0       0       0
#> Morocco                                         0       0       0
#> Mozambique                                      0       0       0
#> Myanmar                                         0       0       0
#> Nepal                                           0       0       0
#> Netherlands                                     5       1       1
#> New Zealand                                     0       0       0
#> Nicaragua                                       0       0       0
#> Niger                                           0       0       1
#> Nigeria                                         0      17       1
#> Norway                                          0       0       0
#> Oman                                            0       0       0
#> Pakistan                                        0       0       0
#> Panama                                          0       0       0
#> Papua New Guinea                                0       0       0
#> Paraguay                                        0       2       0
#> Peru                                            0       2       0
#> Philippines                                     0       0       0
#> Poland                                          0       2       0
#> Portugal                                        0       0       0
#> Qatar                                           0       1       0
#> Romania                                         0       0       0
#> Russian Federation                              6       0      19
#> Rwanda                                          0       0       0
#> Saudi Arabia                                    0       8       0
#> Senegal                                         0       0       0
#> Sierra Leone                                    0       0       3
#> Singapore                                       0       0       0
#> Slovakia                                        0       0       0
#> Slovenia                                        0       0       0
#> Solomon Islands                                 0       0       0
#> Somalia                                         0       0       0
#> South Africa                                    0       0       1
#> Spain                                           0       8       0
#> Sri Lanka                                       0       2       0
#> Sudan                                           8      10       1
#> Suriname                                        0       0       0
#> Sweden                                          2       1       0
#> Switzerland                                     0       1       0
#> Syrian Arab Republic                            0      28       0
#> Tajikistan                                      0       0       0
#> Tanzania, United Republic Of                    0       0       0
#> Thailand                                        0       1       0
#> Togo                                            0       0       0
#> Trinidad And Tobago                             0       0       0
#> Tunisia                                         0       0       0
#> Turkmenistan                                    0       0       0
#> Uganda                                          0       1       0
#> Ukraine                                         0       0       0
#> United Arab Emirates                            0       5       0
#> United Kingdom                                  3       5       1
#> United States                                   0      42      11
#> Uruguay                                         0       0       0
#> Uzbekistan                                      0       0       0
#> Vietnam                                         0       0       0
#> Yemen                                           0       5       0
#> Zambia                                          0       0       0
#> Zimbabwe                                        0       0       0
#>                                            Libyan Arab Jamahiriya Lithuania
#> Afghanistan                                                     0         2
#> Albania                                                         0         0
#> Algeria                                                         1         0
#> Angola                                                          0         0
#> Argentina                                                       0         0
#> Armenia                                                         0         0
#> Australia                                                       0         0
#> Austria                                                         0         2
#> Azerbaijan                                                      0         0
#> Bahrain                                                         0         0
#> Bangladesh                                                      0         0
#> Belarus                                                         0         2
#> Belgium                                                         0         2
#> Benin                                                           0         0
#> Bolivia, Plurinational State Of                                 0         0
#> Bosnia And Herzegovina                                          0         0
#> Brazil                                                          0         0
#> Bulgaria                                                        0         0
#> Burkina Faso                                                    0         0
#> Burundi                                                         0         0
#> Cambodia                                                        0         0
#> Cameroon                                                        0         0
#> Canada                                                          3         0
#> Cape Verde                                                      0         0
#> Central African Republic                                        0         0
#> Chad                                                            0         0
#> Chile                                                           0         0
#> China                                                           0         0
#> Colombia                                                        0         0
#> Comoros                                                         2         0
#> Congo, Republic Of                                              0         0
#> Congo, The Democratic Republic Of                               0         0
#> Costa Rica                                                      0         0
#> Cote D'ivoire                                                   0         0
#> Croatia                                                         0         0
#> Cuba                                                            1         0
#> Cyprus                                                          0         0
#> Denmark                                                         0         0
#> Djibouti                                                        0         0
#> Dominican Republic                                              0         0
#> Ecuador                                                         0         0
#> Egypt                                                           2         0
#> El Salvador                                                     0         0
#> Equatorial Guinea                                               0         0
#> Estonia                                                         0         2
#> Ethiopia                                                        4         0
#> Fiji                                                            0         0
#> Finland                                                         0         0
#> France                                                          3         1
#> Gabon                                                           0         0
#> Gambia                                                          0         0
#> Georgia                                                         0         1
#> Germany                                                         5         5
#> Ghana                                                           2         0
#> Greece                                                          3         0
#> Guatemala                                                       0         0
#> Guinea                                                          0         0
#> Guinea-Bissau                                                   0         0
#> Guyana                                                          0         0
#> Haiti                                                           0         0
#> Honduras                                                        0         0
#> Hungary                                                         0         0
#> India                                                           0         0
#> Indonesia                                                       0         0
#> Iran, Islamic Republic Of                                       1         0
#> Iraq                                                            2         0
#> Ireland                                                         1         2
#> Israel                                                          2         0
#> Italy                                                           4         2
#> Jamaica                                                         0         0
#> Japan                                                           0         0
#> Jordan                                                          0         0
#> Kazakhstan                                                      0         0
#> Kenya                                                           0         0
#> Korea, Democratic People's Republic Of                          1         0
#> Korea, Republic Of                                             32         0
#> Kuwait                                                          0         0
#> Kyrgyzstan                                                      0         1
#> Lao People's Democratic Republic                                0         0
#> Latvia                                                          0         1
#> Lebanon                                                         5         1
#> Liberia                                                         0         0
#> Libyan Arab Jamahiriya                                         NA         0
#> Lithuania                                                       0        NA
#> Luxembourg                                                      0         0
#> Macedonia, The Former Yugoslav Republic Of                      0         0
#> Madagascar                                                      0         0
#> Malawi                                                          0         0
#> Malaysia                                                        0         0
#> Mali                                                            1         0
#> Mauritania                                                      0         0
#> Mauritius                                                       0         0
#> Mexico                                                          0         0
#> Moldova, Republic Of                                            0         2
#> Mongolia                                                        0         0
#> Morocco                                                         0         0
#> Mozambique                                                      0         0
#> Myanmar                                                         0         0
#> Nepal                                                           2         0
#> Netherlands                                                     0         0
#> New Zealand                                                     0         0
#> Nicaragua                                                       0         0
#> Niger                                                           8         0
#> Nigeria                                                        18         1
#> Norway                                                          0         3
#> Oman                                                            0         0
#> Pakistan                                                        1         0
#> Panama                                                          0         0
#> Papua New Guinea                                                0         0
#> Paraguay                                                        0         0
#> Peru                                                            0         0
#> Philippines                                                     0         0
#> Poland                                                          0         3
#> Portugal                                                        0         0
#> Qatar                                                           1         0
#> Romania                                                         0         0
#> Russian Federation                                              0         1
#> Rwanda                                                          0         0
#> Saudi Arabia                                                    0         0
#> Senegal                                                         0         0
#> Sierra Leone                                                    0         0
#> Singapore                                                       0         0
#> Slovakia                                                        0         1
#> Slovenia                                                        0         0
#> Solomon Islands                                                 0         0
#> Somalia                                                         0         0
#> South Africa                                                    0         0
#> Spain                                                           0         0
#> Sri Lanka                                                       0         0
#> Sudan                                                          17         0
#> Suriname                                                        0         0
#> Sweden                                                          0         0
#> Switzerland                                                    13         0
#> Syrian Arab Republic                                            0         0
#> Tajikistan                                                      0         0
#> Tanzania, United Republic Of                                    1         0
#> Thailand                                                        0         0
#> Togo                                                            0         0
#> Trinidad And Tobago                                             0         0
#> Tunisia                                                         1         0
#> Turkmenistan                                                    0         0
#> Uganda                                                          0         0
#> Ukraine                                                         0         0
#> United Arab Emirates                                            0         0
#> United Kingdom                                                  1         1
#> United States                                                   7         1
#> Uruguay                                                         0         0
#> Uzbekistan                                                      0         0
#> Vietnam                                                         0         0
#> Yemen                                                           0         0
#> Zambia                                                          0         0
#> Zimbabwe                                                        2         0
#>                                            Luxembourg
#> Afghanistan                                         0
#> Albania                                             0
#> Algeria                                             0
#> Angola                                              0
#> Argentina                                           0
#> Armenia                                             0
#> Australia                                           0
#> Austria                                             0
#> Azerbaijan                                          0
#> Bahrain                                             0
#> Bangladesh                                          0
#> Belarus                                             0
#> Belgium                                             0
#> Benin                                               0
#> Bolivia, Plurinational State Of                     0
#> Bosnia And Herzegovina                              0
#> Brazil                                              0
#> Bulgaria                                            0
#> Burkina Faso                                        0
#> Burundi                                             0
#> Cambodia                                            0
#> Cameroon                                            0
#> Canada                                              0
#> Cape Verde                                          0
#> Central African Republic                            0
#> Chad                                                0
#> Chile                                               0
#> China                                               0
#> Colombia                                            0
#> Comoros                                             0
#> Congo, Republic Of                                  0
#> Congo, The Democratic Republic Of                   0
#> Costa Rica                                          0
#> Cote D'ivoire                                       0
#> Croatia                                             0
#> Cuba                                                0
#> Cyprus                                              0
#> Denmark                                             1
#> Djibouti                                            0
#> Dominican Republic                                  0
#> Ecuador                                             0
#> Egypt                                               0
#> El Salvador                                         0
#> Equatorial Guinea                                   0
#> Estonia                                             0
#> Ethiopia                                            0
#> Fiji                                                0
#> Finland                                             0
#> France                                              1
#> Gabon                                               0
#> Gambia                                              0
#> Georgia                                             0
#> Germany                                             0
#> Ghana                                               0
#> Greece                                              0
#> Guatemala                                           0
#> Guinea                                              0
#> Guinea-Bissau                                       0
#> Guyana                                              0
#> Haiti                                               0
#> Honduras                                            0
#> Hungary                                             0
#> India                                               0
#> Indonesia                                           0
#> Iran, Islamic Republic Of                           0
#> Iraq                                                0
#> Ireland                                             0
#> Israel                                              0
#> Italy                                               0
#> Jamaica                                             0
#> Japan                                               0
#> Jordan                                              0
#> Kazakhstan                                          0
#> Kenya                                               0
#> Korea, Democratic People's Republic Of              0
#> Korea, Republic Of                                  0
#> Kuwait                                              0
#> Kyrgyzstan                                          0
#> Lao People's Democratic Republic                    0
#> Latvia                                              0
#> Lebanon                                             0
#> Liberia                                             0
#> Libyan Arab Jamahiriya                              0
#> Lithuania                                           0
#> Luxembourg                                         NA
#> Macedonia, The Former Yugoslav Republic Of          0
#> Madagascar                                          0
#> Malawi                                              0
#> Malaysia                                            0
#> Mali                                                0
#> Mauritania                                          0
#> Mauritius                                           0
#> Mexico                                              0
#> Moldova, Republic Of                                0
#> Mongolia                                            0
#> Morocco                                             0
#> Mozambique                                          0
#> Myanmar                                             0
#> Nepal                                               0
#> Netherlands                                         1
#> New Zealand                                         0
#> Nicaragua                                           0
#> Niger                                               0
#> Nigeria                                             0
#> Norway                                              0
#> Oman                                                0
#> Pakistan                                            0
#> Panama                                              0
#> Papua New Guinea                                    0
#> Paraguay                                            0
#> Peru                                                0
#> Philippines                                         0
#> Poland                                              0
#> Portugal                                            0
#> Qatar                                               0
#> Romania                                             0
#> Russian Federation                                  0
#> Rwanda                                              0
#> Saudi Arabia                                        0
#> Senegal                                             0
#> Sierra Leone                                        0
#> Singapore                                           0
#> Slovakia                                            0
#> Slovenia                                            0
#> Solomon Islands                                     0
#> Somalia                                             0
#> South Africa                                        0
#> Spain                                               0
#> Sri Lanka                                           0
#> Sudan                                               0
#> Suriname                                            0
#> Sweden                                              1
#> Switzerland                                         0
#> Syrian Arab Republic                                0
#> Tajikistan                                          0
#> Tanzania, United Republic Of                        0
#> Thailand                                            0
#> Togo                                                0
#> Trinidad And Tobago                                 0
#> Tunisia                                             0
#> Turkmenistan                                        0
#> Uganda                                              0
#> Ukraine                                             0
#> United Arab Emirates                                0
#> United Kingdom                                      0
#> United States                                       0
#> Uruguay                                             0
#> Uzbekistan                                          0
#> Vietnam                                             0
#> Yemen                                               0
#> Zambia                                              0
#> Zimbabwe                                            0
#>                                            Macedonia, The Former Yugoslav Republic Of
#> Afghanistan                                                                         5
#> Albania                                                                             4
#> Algeria                                                                             0
#> Angola                                                                              0
#> Argentina                                                                           0
#> Armenia                                                                             0
#> Australia                                                                           0
#> Austria                                                                             0
#> Azerbaijan                                                                          0
#> Bahrain                                                                             0
#> Bangladesh                                                                          0
#> Belarus                                                                             0
#> Belgium                                                                             1
#> Benin                                                                               0
#> Bolivia, Plurinational State Of                                                     0
#> Bosnia And Herzegovina                                                              0
#> Brazil                                                                              0
#> Bulgaria                                                                            1
#> Burkina Faso                                                                        0
#> Burundi                                                                             0
#> Cambodia                                                                            0
#> Cameroon                                                                            0
#> Canada                                                                              0
#> Cape Verde                                                                          0
#> Central African Republic                                                            0
#> Chad                                                                                0
#> Chile                                                                               0
#> China                                                                               0
#> Colombia                                                                            0
#> Comoros                                                                             0
#> Congo, Republic Of                                                                  0
#> Congo, The Democratic Republic Of                                                   0
#> Costa Rica                                                                          0
#> Cote D'ivoire                                                                       0
#> Croatia                                                                             0
#> Cuba                                                                                0
#> Cyprus                                                                              0
#> Denmark                                                                             0
#> Djibouti                                                                            0
#> Dominican Republic                                                                  0
#> Ecuador                                                                             0
#> Egypt                                                                               0
#> El Salvador                                                                         0
#> Equatorial Guinea                                                                   0
#> Estonia                                                                             0
#> Ethiopia                                                                            0
#> Fiji                                                                                0
#> Finland                                                                             0
#> France                                                                              2
#> Gabon                                                                               0
#> Gambia                                                                              0
#> Georgia                                                                             0
#> Germany                                                                             0
#> Ghana                                                                               0
#> Greece                                                                              1
#> Guatemala                                                                           0
#> Guinea                                                                              0
#> Guinea-Bissau                                                                       0
#> Guyana                                                                              0
#> Haiti                                                                               0
#> Honduras                                                                            0
#> Hungary                                                                             0
#> India                                                                               0
#> Indonesia                                                                           0
#> Iran, Islamic Republic Of                                                           0
#> Iraq                                                                                0
#> Ireland                                                                             0
#> Israel                                                                              0
#> Italy                                                                               0
#> Jamaica                                                                             0
#> Japan                                                                               0
#> Jordan                                                                              0
#> Kazakhstan                                                                          0
#> Kenya                                                                               0
#> Korea, Democratic People's Republic Of                                              0
#> Korea, Republic Of                                                                  0
#> Kuwait                                                                              0
#> Kyrgyzstan                                                                          0
#> Lao People's Democratic Republic                                                    0
#> Latvia                                                                              0
#> Lebanon                                                                             0
#> Liberia                                                                             0
#> Libyan Arab Jamahiriya                                                              0
#> Lithuania                                                                           0
#> Luxembourg                                                                          0
#> Macedonia, The Former Yugoslav Republic Of                                         NA
#> Madagascar                                                                          0
#> Malawi                                                                              0
#> Malaysia                                                                            0
#> Mali                                                                                0
#> Mauritania                                                                          0
#> Mauritius                                                                           0
#> Mexico                                                                              0
#> Moldova, Republic Of                                                                0
#> Mongolia                                                                            0
#> Morocco                                                                             0
#> Mozambique                                                                          0
#> Myanmar                                                                             0
#> Nepal                                                                               0
#> Netherlands                                                                         0
#> New Zealand                                                                         0
#> Nicaragua                                                                           0
#> Niger                                                                               0
#> Nigeria                                                                             0
#> Norway                                                                              2
#> Oman                                                                                0
#> Pakistan                                                                            0
#> Panama                                                                              0
#> Papua New Guinea                                                                    0
#> Paraguay                                                                            0
#> Peru                                                                                0
#> Philippines                                                                         0
#> Poland                                                                              1
#> Portugal                                                                            1
#> Qatar                                                                               0
#> Romania                                                                             1
#> Russian Federation                                                                  0
#> Rwanda                                                                              0
#> Saudi Arabia                                                                        0
#> Senegal                                                                             0
#> Sierra Leone                                                                        0
#> Singapore                                                                           0
#> Slovakia                                                                            1
#> Slovenia                                                                            0
#> Solomon Islands                                                                     0
#> Somalia                                                                             0
#> South Africa                                                                        0
#> Spain                                                                               0
#> Sri Lanka                                                                           0
#> Sudan                                                                               0
#> Suriname                                                                            0
#> Sweden                                                                              0
#> Switzerland                                                                         0
#> Syrian Arab Republic                                                                0
#> Tajikistan                                                                          0
#> Tanzania, United Republic Of                                                        0
#> Thailand                                                                            0
#> Togo                                                                                0
#> Trinidad And Tobago                                                                 0
#> Tunisia                                                                             0
#> Turkmenistan                                                                        0
#> Uganda                                                                              0
#> Ukraine                                                                             0
#> United Arab Emirates                                                                0
#> United Kingdom                                                                      0
#> United States                                                                       0
#> Uruguay                                                                             0
#> Uzbekistan                                                                          0
#> Vietnam                                                                             0
#> Yemen                                                                               0
#> Zambia                                                                              0
#> Zimbabwe                                                                            0
#>                                            Madagascar Malawi Malaysia Mali
#> Afghanistan                                         0      0        1    0
#> Albania                                             0      0        0    0
#> Algeria                                             0      0        1    4
#> Angola                                              0      0        0    0
#> Argentina                                           0      0        0    0
#> Armenia                                             0      0        0    0
#> Australia                                           0      0        7    0
#> Austria                                             0      0        0    0
#> Azerbaijan                                          0      0        0    0
#> Bahrain                                             0      0        0    0
#> Bangladesh                                          0      0        1    0
#> Belarus                                             0      0        0    0
#> Belgium                                             0      0        0    0
#> Benin                                               0      0        0    0
#> Bolivia, Plurinational State Of                     0      0        0    0
#> Bosnia And Herzegovina                              0      0        0    0
#> Brazil                                              0      0        0    0
#> Bulgaria                                            0      0        0    0
#> Burkina Faso                                        0      0        0    0
#> Burundi                                             0      0        0    0
#> Cambodia                                            0      0        0    0
#> Cameroon                                            0      0        0    0
#> Canada                                              0      0        0    0
#> Cape Verde                                          0      0        0    0
#> Central African Republic                            0      0        0    0
#> Chad                                                0      0        0    0
#> Chile                                               0      0        0    0
#> China                                               0      3        1    0
#> Colombia                                            0      0        0    0
#> Comoros                                             0      0        0    0
#> Congo, Republic Of                                  0      0        0    0
#> Congo, The Democratic Republic Of                   0      0        0    0
#> Costa Rica                                          0      0        0    0
#> Cote D'ivoire                                       0      0        0    0
#> Croatia                                             0      0        0    0
#> Cuba                                                0      0        0    0
#> Cyprus                                              0      0        0    0
#> Denmark                                             0      0        0    0
#> Djibouti                                            0      0        0    0
#> Dominican Republic                                  0      0        0    0
#> Ecuador                                             0      0        0    0
#> Egypt                                               0      0       10    0
#> El Salvador                                         0      0        0    0
#> Equatorial Guinea                                   0      0        0    0
#> Estonia                                             0      0        0    0
#> Ethiopia                                            0      0        0    0
#> Fiji                                                0      0        0    0
#> Finland                                             0      0        0    0
#> France                                              0      0        1    3
#> Gabon                                               0      0        0    0
#> Gambia                                              0      0        1    0
#> Georgia                                             0      0       14    0
#> Germany                                             0      0        0    0
#> Ghana                                               0      0        0    0
#> Greece                                              0      0        0    0
#> Guatemala                                           0      0        0    0
#> Guinea                                              0      0        0    0
#> Guinea-Bissau                                       0      0        0    0
#> Guyana                                              0      0        0    0
#> Haiti                                               0      0        0    0
#> Honduras                                            0      0        0    0
#> Hungary                                             0      0        0    0
#> India                                               0      0        9    0
#> Indonesia                                           0      0       68    0
#> Iran, Islamic Republic Of                           0      0       36    0
#> Iraq                                                0      0        2    0
#> Ireland                                             0      0        0    0
#> Israel                                              0      0        7    0
#> Italy                                               0      0        0    2
#> Jamaica                                             0      0        0    0
#> Japan                                               0      0        0    0
#> Jordan                                              0      0        0    0
#> Kazakhstan                                          0      0        0    0
#> Kenya                                               0      0        0    0
#> Korea, Democratic People's Republic Of              0      0        0    0
#> Korea, Republic Of                                  0      0        1    0
#> Kuwait                                              0      0        0    0
#> Kyrgyzstan                                          0      0        0    0
#> Lao People's Democratic Republic                    0      0        0    0
#> Latvia                                              0      0        0    0
#> Lebanon                                             0      0        0    0
#> Liberia                                             0      0        0    0
#> Libyan Arab Jamahiriya                              0      0        0    1
#> Lithuania                                           0      0        0    0
#> Luxembourg                                          0      0        0    0
#> Macedonia, The Former Yugoslav Republic Of          0      0        0    0
#> Madagascar                                         NA      0        0    0
#> Malawi                                              0     NA        0    0
#> Malaysia                                            0      0       NA    0
#> Mali                                                0      0        0   NA
#> Mauritania                                          0      0        0   62
#> Mauritius                                           0      0        0    0
#> Mexico                                              0      0        1    0
#> Moldova, Republic Of                                0      0        0    0
#> Mongolia                                            0      0        0    0
#> Morocco                                             0      0        0    0
#> Mozambique                                          0      6        0    0
#> Myanmar                                             0      0        2    0
#> Nepal                                               0      0        0    0
#> Netherlands                                         0      0        0    0
#> New Zealand                                         0      0        0    0
#> Nicaragua                                           0      0        0    0
#> Niger                                               2      1        0    2
#> Nigeria                                             0      0        6    0
#> Norway                                              0      0        0    0
#> Oman                                                0      0        0    0
#> Pakistan                                            0      0        1    0
#> Panama                                              0      0        0    0
#> Papua New Guinea                                    0      0        0    0
#> Paraguay                                            0      0        0    0
#> Peru                                                0      0        0    0
#> Philippines                                         0      0       10    0
#> Poland                                              0      0        0    0
#> Portugal                                            0      0        0    0
#> Qatar                                               0      0        0    0
#> Romania                                             0      0        0    0
#> Russian Federation                                  0      0        2    0
#> Rwanda                                              0      0        0    0
#> Saudi Arabia                                        0      0        1    0
#> Senegal                                             0      0        0    1
#> Sierra Leone                                        0      0        0    0
#> Singapore                                           0      0       10    0
#> Slovakia                                            0      0        0    0
#> Slovenia                                            0      0        0    0
#> Solomon Islands                                     0      0        0    0
#> Somalia                                             0      2        1    0
#> South Africa                                        0      2        3    0
#> Spain                                               0      0        0    7
#> Sri Lanka                                           0      0        3    0
#> Sudan                                               0      0        0    0
#> Suriname                                            0      0        0    0
#> Sweden                                              0      0        1    0
#> Switzerland                                         0      0        0    0
#> Syrian Arab Republic                                0      0        0    0
#> Tajikistan                                          0      0        0    0
#> Tanzania, United Republic Of                        0      0        0    0
#> Thailand                                            0      0       16    0
#> Togo                                                0      0        0    0
#> Trinidad And Tobago                                 0      0        0    0
#> Tunisia                                             0      0        0    0
#> Turkmenistan                                        0      0        0    0
#> Uganda                                              0      0        0    0
#> Ukraine                                             0      0        0    0
#> United Arab Emirates                                0      0        0    0
#> United Kingdom                                      0      1        1    0
#> United States                                       0      0        4    2
#> Uruguay                                             0      0        0    0
#> Uzbekistan                                          0      0        0    0
#> Vietnam                                             0      0        0    0
#> Yemen                                               0      0        5    0
#> Zambia                                              0      1        0    0
#> Zimbabwe                                            0      0        0    0
#>                                            Mauritania Mauritius Mexico
#> Afghanistan                                         1         0      0
#> Albania                                             0         0      0
#> Algeria                                            15         1      0
#> Angola                                              0         0      0
#> Argentina                                           0         0      2
#> Armenia                                             0         0      0
#> Australia                                           0         0      3
#> Austria                                             0         0      0
#> Azerbaijan                                          0         0      0
#> Bahrain                                             0         0      0
#> Bangladesh                                          0         0      0
#> Belarus                                             0         0      0
#> Belgium                                             0         0      0
#> Benin                                               0         0      0
#> Bolivia, Plurinational State Of                     0         0      4
#> Bosnia And Herzegovina                              0         0      0
#> Brazil                                              0         0      8
#> Bulgaria                                            0         0      0
#> Burkina Faso                                        0         0      0
#> Burundi                                             0         0      0
#> Cambodia                                            0         0      0
#> Cameroon                                            0         0      0
#> Canada                                              0         0      5
#> Cape Verde                                          0         0      0
#> Central African Republic                            0         0      0
#> Chad                                                0         0      0
#> Chile                                               0         0      7
#> China                                               1         0      4
#> Colombia                                            0         0     18
#> Comoros                                             0         0      0
#> Congo, Republic Of                                  0         0      0
#> Congo, The Democratic Republic Of                   0         0      0
#> Costa Rica                                          0         0      1
#> Cote D'ivoire                                       0         0      0
#> Croatia                                             0         0      0
#> Cuba                                                0         0      1
#> Cyprus                                              0         0      0
#> Denmark                                             0         0      0
#> Djibouti                                            0         0      0
#> Dominican Republic                                  0         0      1
#> Ecuador                                             0         0      6
#> Egypt                                               0         0      0
#> El Salvador                                         0         0      8
#> Equatorial Guinea                                   0         0      0
#> Estonia                                             0         0      0
#> Ethiopia                                            0         0      0
#> Fiji                                                0         0      0
#> Finland                                             0         0      0
#> France                                             14         0      1
#> Gabon                                               0         0      0
#> Gambia                                              1         0      0
#> Georgia                                             0         0      2
#> Germany                                             0         0      2
#> Ghana                                               0         0      0
#> Greece                                              0         0      0
#> Guatemala                                           0         0      9
#> Guinea                                              1         0      0
#> Guinea-Bissau                                       0         0      0
#> Guyana                                              0         0      0
#> Haiti                                               0         0      1
#> Honduras                                            0         0     14
#> Hungary                                             0         0      1
#> India                                               0         0      5
#> Indonesia                                           0         0      0
#> Iran, Islamic Republic Of                           0         0      0
#> Iraq                                                0         0      0
#> Ireland                                             0         0      0
#> Israel                                             30         0      1
#> Italy                                               1         0      0
#> Jamaica                                             0         0      0
#> Japan                                               0         0      0
#> Jordan                                              0         0      0
#> Kazakhstan                                          0         0      0
#> Kenya                                               0         0      0
#> Korea, Democratic People's Republic Of              0         0      0
#> Korea, Republic Of                                  0         0      0
#> Kuwait                                              0         0      1
#> Kyrgyzstan                                          0         0      0
#> Lao People's Democratic Republic                    0         0      0
#> Latvia                                              0         0      0
#> Lebanon                                             0         0      3
#> Liberia                                             0         0      0
#> Libyan Arab Jamahiriya                              0         0      0
#> Lithuania                                           0         0      0
#> Luxembourg                                          0         0      0
#> Macedonia, The Former Yugoslav Republic Of          0         0      0
#> Madagascar                                          0         0      0
#> Malawi                                              0         0      0
#> Malaysia                                            0         0      1
#> Mali                                               62         0      0
#> Mauritania                                         NA         0      0
#> Mauritius                                           0        NA      0
#> Mexico                                              0         0     NA
#> Moldova, Republic Of                                0         0      0
#> Mongolia                                            0         0      0
#> Morocco                                             1         0      0
#> Mozambique                                          0         0      0
#> Myanmar                                             0         0      0
#> Nepal                                               0         0      0
#> Netherlands                                         0         0      6
#> New Zealand                                         0         0      0
#> Nicaragua                                           0         0      6
#> Niger                                               0         0      0
#> Nigeria                                             0         0      0
#> Norway                                              0         0      0
#> Oman                                                0         0      0
#> Pakistan                                            0         0      1
#> Panama                                              0         0      2
#> Papua New Guinea                                    0         0      1
#> Paraguay                                            0         0      1
#> Peru                                                0         0      2
#> Philippines                                         0         0      1
#> Poland                                              0         0      0
#> Portugal                                            0         0      0
#> Qatar                                               1         0      0
#> Romania                                             0         0      0
#> Russian Federation                                  0         0      1
#> Rwanda                                              0         0      0
#> Saudi Arabia                                        0         0      0
#> Senegal                                             1         0      0
#> Sierra Leone                                        0         0      0
#> Singapore                                           0         0      1
#> Slovakia                                            0         0      0
#> Slovenia                                            0         0      0
#> Solomon Islands                                     0         0      0
#> Somalia                                             0         0      0
#> South Africa                                        0         0      0
#> Spain                                               9         0      7
#> Sri Lanka                                           0         0      0
#> Sudan                                               0         0      0
#> Suriname                                            0         0      0
#> Sweden                                              0         0      0
#> Switzerland                                         0         0      0
#> Syrian Arab Republic                                0         0      0
#> Tajikistan                                          0         0      0
#> Tanzania, United Republic Of                        0         0      0
#> Thailand                                            0         0      3
#> Togo                                                0         0      0
#> Trinidad And Tobago                                 0         0      0
#> Tunisia                                             1         0      0
#> Turkmenistan                                        0         0      0
#> Uganda                                              1         0      0
#> Ukraine                                             0         0      0
#> United Arab Emirates                                0         0      1
#> United Kingdom                                      0         0      0
#> United States                                       0         1    211
#> Uruguay                                             0         0      2
#> Uzbekistan                                          0         0      0
#> Vietnam                                             0         0      0
#> Yemen                                               0         0      0
#> Zambia                                              0         0      0
#> Zimbabwe                                            0         0      1
#>                                            Moldova, Republic Of Mongolia
#> Afghanistan                                                   2        0
#> Albania                                                       0        0
#> Algeria                                                       0        0
#> Angola                                                        0        0
#> Argentina                                                     0        0
#> Armenia                                                       1        0
#> Australia                                                     0        0
#> Austria                                                       0        0
#> Azerbaijan                                                    0        0
#> Bahrain                                                       0        0
#> Bangladesh                                                    0        0
#> Belarus                                                       0        0
#> Belgium                                                       0        0
#> Benin                                                         0        0
#> Bolivia, Plurinational State Of                               0        0
#> Bosnia And Herzegovina                                        0        0
#> Brazil                                                        0        0
#> Bulgaria                                                      0        0
#> Burkina Faso                                                  0        0
#> Burundi                                                       0        0
#> Cambodia                                                      0        0
#> Cameroon                                                      0        0
#> Canada                                                        0        0
#> Cape Verde                                                    0        0
#> Central African Republic                                      0        0
#> Chad                                                          0        0
#> Chile                                                         0        0
#> China                                                         0        4
#> Colombia                                                      0        0
#> Comoros                                                       0        0
#> Congo, Republic Of                                            0        1
#> Congo, The Democratic Republic Of                             0        0
#> Costa Rica                                                    0        1
#> Cote D'ivoire                                                 0        0
#> Croatia                                                       0        0
#> Cuba                                                          0        0
#> Cyprus                                                        0        0
#> Denmark                                                       0        0
#> Djibouti                                                      0        0
#> Dominican Republic                                            0        0
#> Ecuador                                                       0        0
#> Egypt                                                         2        0
#> El Salvador                                                   0        0
#> Equatorial Guinea                                             0        0
#> Estonia                                                       0        0
#> Ethiopia                                                      0        0
#> Fiji                                                          0        0
#> Finland                                                       0        0
#> France                                                        0        0
#> Gabon                                                         0        0
#> Gambia                                                        0        0
#> Georgia                                                       0        0
#> Germany                                                       0        0
#> Ghana                                                         0        0
#> Greece                                                        0        0
#> Guatemala                                                     0        0
#> Guinea                                                        0        0
#> Guinea-Bissau                                                 0        0
#> Guyana                                                        0        0
#> Haiti                                                         0        0
#> Honduras                                                      0        0
#> Hungary                                                       0        0
#> India                                                         0        0
#> Indonesia                                                     0        0
#> Iran, Islamic Republic Of                                     0        0
#> Iraq                                                          0        0
#> Ireland                                                       0        0
#> Israel                                                        0        0
#> Italy                                                         0        0
#> Jamaica                                                       0        0
#> Japan                                                         0        6
#> Jordan                                                        0        0
#> Kazakhstan                                                    0        0
#> Kenya                                                         0        0
#> Korea, Democratic People's Republic Of                        0        0
#> Korea, Republic Of                                            0        1
#> Kuwait                                                        0        0
#> Kyrgyzstan                                                    0        0
#> Lao People's Democratic Republic                              0        0
#> Latvia                                                        0        0
#> Lebanon                                                       0        0
#> Liberia                                                       0        0
#> Libyan Arab Jamahiriya                                        0        0
#> Lithuania                                                     2        0
#> Luxembourg                                                    0        0
#> Macedonia, The Former Yugoslav Republic Of                    0        0
#> Madagascar                                                    0        0
#> Malawi                                                        0        0
#> Malaysia                                                      0        0
#> Mali                                                          0        0
#> Mauritania                                                    0        0
#> Mauritius                                                     0        0
#> Mexico                                                        0        0
#> Moldova, Republic Of                                         NA        0
#> Mongolia                                                      0       NA
#> Morocco                                                       0        0
#> Mozambique                                                    0        0
#> Myanmar                                                       0        0
#> Nepal                                                         0        0
#> Netherlands                                                   0        0
#> New Zealand                                                   0        0
#> Nicaragua                                                     0        0
#> Niger                                                         0        0
#> Nigeria                                                       0        0
#> Norway                                                        0        0
#> Oman                                                          0        0
#> Pakistan                                                      0        1
#> Panama                                                        0        0
#> Papua New Guinea                                              0        0
#> Paraguay                                                      0        0
#> Peru                                                          0        0
#> Philippines                                                   0        0
#> Poland                                                        0        0
#> Portugal                                                      0        0
#> Qatar                                                         0        0
#> Romania                                                       1        0
#> Russian Federation                                           11        2
#> Rwanda                                                        0        0
#> Saudi Arabia                                                  0        0
#> Senegal                                                       0        0
#> Sierra Leone                                                  0        1
#> Singapore                                                     0        0
#> Slovakia                                                      0        0
#> Slovenia                                                      0        0
#> Solomon Islands                                               0        0
#> Somalia                                                       0        0
#> South Africa                                                  0        0
#> Spain                                                         0        0
#> Sri Lanka                                                     0        0
#> Sudan                                                         0        0
#> Suriname                                                      0        0
#> Sweden                                                        0        0
#> Switzerland                                                   0        0
#> Syrian Arab Republic                                          0        0
#> Tajikistan                                                    0        0
#> Tanzania, United Republic Of                                  0        0
#> Thailand                                                      0        0
#> Togo                                                          0        0
#> Trinidad And Tobago                                           0        0
#> Tunisia                                                       0        0
#> Turkmenistan                                                  0        0
#> Uganda                                                        0        0
#> Ukraine                                                       2        1
#> United Arab Emirates                                          0        0
#> United Kingdom                                                0        0
#> United States                                                 0        1
#> Uruguay                                                       0        0
#> Uzbekistan                                                    0        0
#> Vietnam                                                       0        0
#> Yemen                                                         0        0
#> Zambia                                                        0        0
#> Zimbabwe                                                      0        0
#>                                            Morocco Mozambique Myanmar Nepal
#> Afghanistan                                      0          0       0     2
#> Albania                                          0          0       0     0
#> Algeria                                          1          0       0     0
#> Angola                                           0          0       0     0
#> Argentina                                        0          0       0     0
#> Armenia                                          0          0       0     0
#> Australia                                        0          0       5     0
#> Austria                                          0          0       0     0
#> Azerbaijan                                       0          0       0     0
#> Bahrain                                          0          0       0     0
#> Bangladesh                                       0          0       3    16
#> Belarus                                          0          0       0     0
#> Belgium                                          1          0       0     0
#> Benin                                            0          0       0     0
#> Bolivia, Plurinational State Of                  0          0       0     0
#> Bosnia And Herzegovina                           0          0       0     0
#> Brazil                                           0          0       0     0
#> Bulgaria                                         0          0       0     0
#> Burkina Faso                                     0          0       0     0
#> Burundi                                          0          0       0     0
#> Cambodia                                         0          0       0     0
#> Cameroon                                         0          0       3     0
#> Canada                                           0          0       0     0
#> Cape Verde                                       0          0       0     0
#> Central African Republic                         0          0       0     0
#> Chad                                             0          0       0     0
#> Chile                                            0          0       0     0
#> China                                            0          0       2    31
#> Colombia                                         0          0       0     0
#> Comoros                                          0          0       0     0
#> Congo, Republic Of                               0          0       0     0
#> Congo, The Democratic Republic Of                0          0       0     0
#> Costa Rica                                       0          0       0     0
#> Cote D'ivoire                                    0          0       0     0
#> Croatia                                          0          0       0     0
#> Cuba                                             0          0       0     0
#> Cyprus                                           0          0       0     0
#> Denmark                                          0          0       0     0
#> Djibouti                                         0          0       0     0
#> Dominican Republic                               0          0       0     0
#> Ecuador                                          0          0       0     0
#> Egypt                                            0          0       0     0
#> El Salvador                                      0          0       0     0
#> Equatorial Guinea                                0          0       0     0
#> Estonia                                          0          0       0     0
#> Ethiopia                                         0          0       0     0
#> Fiji                                             0          0       0     0
#> Finland                                          0          1       0     0
#> France                                           4          0       2     1
#> Gabon                                            0          0       0     0
#> Gambia                                           0          0       0     0
#> Georgia                                          0          0       0     0
#> Germany                                          2          0       0     2
#> Ghana                                            0          0       0     0
#> Greece                                           0          0       0     0
#> Guatemala                                        0          0       0     0
#> Guinea                                           3          0       0     0
#> Guinea-Bissau                                    0          0       0     0
#> Guyana                                           0          0       1     0
#> Haiti                                            0          0       0     0
#> Honduras                                         0          1       0     0
#> Hungary                                          0          0       0     0
#> India                                            0          0      19    30
#> Indonesia                                        0          0       1     0
#> Iran, Islamic Republic Of                        0          0       0     0
#> Iraq                                             0          0       0     0
#> Ireland                                          0          0       0     0
#> Israel                                           7          0       1     3
#> Italy                                            2          0       0     0
#> Jamaica                                          0          0       0     0
#> Japan                                            0          0      14     4
#> Jordan                                           1          0       0     0
#> Kazakhstan                                       0          0       0     0
#> Kenya                                            0          0       0     0
#> Korea, Democratic People's Republic Of           0          0       5     0
#> Korea, Republic Of                               0          0       0    26
#> Kuwait                                           0          0       0     0
#> Kyrgyzstan                                       0          0       1     0
#> Lao People's Democratic Republic                 0          0       0     0
#> Latvia                                           0          0       0     0
#> Lebanon                                          0          0       0     0
#> Liberia                                          0          0       0     0
#> Libyan Arab Jamahiriya                           0          0       0     2
#> Lithuania                                        0          0       0     0
#> Luxembourg                                       0          0       0     0
#> Macedonia, The Former Yugoslav Republic Of       0          0       0     0
#> Madagascar                                       0          0       0     0
#> Malawi                                           0          6       0     0
#> Malaysia                                         0          0       2     0
#> Mali                                             0          0       0     0
#> Mauritania                                       1          0       0     0
#> Mauritius                                        0          0       0     0
#> Mexico                                           0          0       0     0
#> Moldova, Republic Of                             0          0       0     0
#> Mongolia                                         0          0       0     0
#> Morocco                                         NA          0       0     0
#> Mozambique                                       0         NA       0     0
#> Myanmar                                          0          0      NA     0
#> Nepal                                            0          0       0    NA
#> Netherlands                                      1          0       0     0
#> New Zealand                                      0          0       0     0
#> Nicaragua                                        0          0       0     0
#> Niger                                            1          0       0     0
#> Nigeria                                          0          0       0     0
#> Norway                                           0          0       0     2
#> Oman                                             0          0       0     0
#> Pakistan                                         0          2       0     2
#> Panama                                           0          0       0     0
#> Papua New Guinea                                 0          0       0     0
#> Paraguay                                         0          0       0     0
#> Peru                                             0          0       0     0
#> Philippines                                      0          0       0     0
#> Poland                                           0          0       0     0
#> Portugal                                         0          0       0     0
#> Qatar                                            3          0       0     0
#> Romania                                          0          0       0     0
#> Russian Federation                               0          0       0     0
#> Rwanda                                           0          0       0     0
#> Saudi Arabia                                     0          0       0     0
#> Senegal                                          1          0       0     0
#> Sierra Leone                                     0          0       0     0
#> Singapore                                        0          0       0     0
#> Slovakia                                         0          0       0     0
#> Slovenia                                         0          0       0     0
#> Solomon Islands                                  0          0       0     0
#> Somalia                                          0         13       0     0
#> South Africa                                     0         11       0     0
#> Spain                                           21          0       0     0
#> Sri Lanka                                        0          0       0     0
#> Sudan                                            0          1       0     0
#> Suriname                                         0          0       0     0
#> Sweden                                           0          0       0     0
#> Switzerland                                      0          0       2     0
#> Syrian Arab Republic                             0          0       0     0
#> Tajikistan                                       0          0       0     0
#> Tanzania, United Republic Of                     0          0       0     0
#> Thailand                                         0          5      40     0
#> Togo                                             0          0       0     0
#> Trinidad And Tobago                              0          0       0     0
#> Tunisia                                          0          0       0     0
#> Turkmenistan                                     0          0       0     0
#> Uganda                                           0          0       0     0
#> Ukraine                                          0          0       0     0
#> United Arab Emirates                             0          0       0     0
#> United Kingdom                                   0          0       0     1
#> United States                                    0          2      43     0
#> Uruguay                                          0          0       0     0
#> Uzbekistan                                       0          0       0     0
#> Vietnam                                          0          0       0     0
#> Yemen                                            0          0       0     2
#> Zambia                                           0          0       0     0
#> Zimbabwe                                         0          0       0     0
#>                                            Netherlands New Zealand Nicaragua
#> Afghanistan                                         21           2         0
#> Albania                                              1           0         0
#> Algeria                                              0           0         0
#> Angola                                               0           0         0
#> Argentina                                            0           0         0
#> Armenia                                              0           0         0
#> Australia                                            3           4         0
#> Austria                                              0           0         0
#> Azerbaijan                                           0           0         0
#> Bahrain                                              0           0         0
#> Bangladesh                                           0           0         0
#> Belarus                                              0           0         0
#> Belgium                                              3           0         0
#> Benin                                                0           0         0
#> Bolivia, Plurinational State Of                      0           0         0
#> Bosnia And Herzegovina                               1           0         0
#> Brazil                                               1           0         0
#> Bulgaria                                             2           0         0
#> Burkina Faso                                         0           0         0
#> Burundi                                              0           0         0
#> Cambodia                                             0           2         0
#> Cameroon                                             0           0         0
#> Canada                                               0           1         0
#> Cape Verde                                           0           0         0
#> Central African Republic                             0           0         0
#> Chad                                                 0           0         0
#> Chile                                                9           0         0
#> China                                                0           3         0
#> Colombia                                             0           0         7
#> Comoros                                              0           0         0
#> Congo, Republic Of                                   0           0         0
#> Congo, The Democratic Republic Of                    1           0         0
#> Costa Rica                                           0           0         5
#> Cote D'ivoire                                        0           0         0
#> Croatia                                              2           0         0
#> Cuba                                                 0           0         1
#> Cyprus                                               0           0         0
#> Denmark                                              1           0         2
#> Djibouti                                             0           0         0
#> Dominican Republic                                   0           0         0
#> Ecuador                                              0           0         0
#> Egypt                                                1           0         0
#> El Salvador                                          0           0         2
#> Equatorial Guinea                                    0           0         0
#> Estonia                                              0           0         0
#> Ethiopia                                             0           0         0
#> Fiji                                                 0           3         0
#> Finland                                              0           0         0
#> France                                               3           0         0
#> Gabon                                                0           0         0
#> Gambia                                               0           0         0
#> Georgia                                              0           0         1
#> Germany                                              4           3         0
#> Ghana                                                0           0         0
#> Greece                                               0           0         0
#> Guatemala                                            0           0         2
#> Guinea                                               0           0         0
#> Guinea-Bissau                                        0           0         0
#> Guyana                                               0           0         0
#> Haiti                                                0           0         0
#> Honduras                                             0           0         8
#> Hungary                                              0           0         0
#> India                                                1           1         0
#> Indonesia                                            1           3         0
#> Iran, Islamic Republic Of                            1           0         0
#> Iraq                                                 0           1         0
#> Ireland                                              2           0         0
#> Israel                                               2           6        33
#> Italy                                                2           0         0
#> Jamaica                                              0           1         0
#> Japan                                                0           4         0
#> Jordan                                               0           0         0
#> Kazakhstan                                           0           0         0
#> Kenya                                                1           0         0
#> Korea, Democratic People's Republic Of               0           0         0
#> Korea, Republic Of                                   0           0         0
#> Kuwait                                               0           2         0
#> Kyrgyzstan                                           0           0         0
#> Lao People's Democratic Republic                     0           0         0
#> Latvia                                               5           0         0
#> Lebanon                                              1           0         0
#> Liberia                                              1           0         0
#> Libyan Arab Jamahiriya                               0           0         0
#> Lithuania                                            0           0         0
#> Luxembourg                                           1           0         0
#> Macedonia, The Former Yugoslav Republic Of           0           0         0
#> Madagascar                                           0           0         0
#> Malawi                                               0           0         0
#> Malaysia                                             0           0         0
#> Mali                                                 0           0         0
#> Mauritania                                           0           0         0
#> Mauritius                                            0           0         0
#> Mexico                                               6           0         6
#> Moldova, Republic Of                                 0           0         0
#> Mongolia                                             0           0         0
#> Morocco                                              1           0         0
#> Mozambique                                           0           0         0
#> Myanmar                                              0           0         0
#> Nepal                                                0           0         0
#> Netherlands                                         NA           0         0
#> New Zealand                                          0          NA         0
#> Nicaragua                                            0           0        NA
#> Niger                                                0           0         0
#> Nigeria                                              1           0         0
#> Norway                                               0           0         1
#> Oman                                                 0           0         0
#> Pakistan                                             0           0         0
#> Panama                                               1           0         0
#> Papua New Guinea                                     0           0         0
#> Paraguay                                             0           0         0
#> Peru                                                 2           0         0
#> Philippines                                          0           3         0
#> Poland                                               1           0         0
#> Portugal                                             0           0         0
#> Qatar                                                0           0         0
#> Romania                                              0           0         0
#> Russian Federation                                   1           0         0
#> Rwanda                                               4           0         0
#> Saudi Arabia                                         1           0         0
#> Senegal                                              0           0         0
#> Sierra Leone                                         0           0         0
#> Singapore                                            0           0         0
#> Slovakia                                             2           0         0
#> Slovenia                                             0           0         0
#> Solomon Islands                                      0           2         0
#> Somalia                                              1           2         0
#> South Africa                                         9           0         0
#> Spain                                                2           0         0
#> Sri Lanka                                            0           0         0
#> Sudan                                                3           0         0
#> Suriname                                             9           0         0
#> Sweden                                               0           0         0
#> Switzerland                                          2           0         0
#> Syrian Arab Republic                                 0           0         0
#> Tajikistan                                           0           0         0
#> Tanzania, United Republic Of                         0           0         0
#> Thailand                                             0           5         0
#> Togo                                                 0           0         0
#> Trinidad And Tobago                                  0           0         0
#> Tunisia                                              0           0         0
#> Turkmenistan                                         0           0         0
#> Uganda                                               2           0         0
#> Ukraine                                              0           0         0
#> United Arab Emirates                                 0           0         0
#> United Kingdom                                       1           1         0
#> United States                                        8          20         5
#> Uruguay                                              2           0         0
#> Uzbekistan                                           0           0         0
#> Vietnam                                              0           1         0
#> Yemen                                               29           0         0
#> Zambia                                               0           0         0
#> Zimbabwe                                             0           0         0
#>                                            Niger Nigeria Norway Oman Pakistan
#> Afghanistan                                    0       0      2    0      691
#> Albania                                        0       0      0    0        0
#> Algeria                                        6       0      0    0        1
#> Angola                                         0       3      0    0        0
#> Argentina                                      0       0      0    0        0
#> Armenia                                        0       0      0    0        2
#> Australia                                      0       1      1    0        2
#> Austria                                        0       0      0    0        0
#> Azerbaijan                                     0       1      0    0        1
#> Bahrain                                        0       0      0    0        0
#> Bangladesh                                     0       3      0    1        6
#> Belarus                                        0       0      0    0        0
#> Belgium                                        0       0      0    0        0
#> Benin                                          2       6      0    0        0
#> Bolivia, Plurinational State Of                0       0      0    0        0
#> Bosnia And Herzegovina                         0       0      0    0        0
#> Brazil                                         0       2      0    0        1
#> Bulgaria                                       0       0      0    0        0
#> Burkina Faso                                   0       0      0    0        0
#> Burundi                                        0       0      0    0        0
#> Cambodia                                       0       4      1    0        0
#> Cameroon                                       0       2      0    0        0
#> Canada                                         0       1      0    0        1
#> Cape Verde                                     0       3      0    0        0
#> Central African Republic                       0       0      0    0        0
#> Chad                                           0       1      0    0        0
#> Chile                                          0       0      0    0       19
#> China                                          0       5      9    2        7
#> Colombia                                       0       3      0    0        9
#> Comoros                                        0       0      0    0        0
#> Congo, Republic Of                             0       1      1    0        0
#> Congo, The Democratic Republic Of              0       0      8    0        0
#> Costa Rica                                     0       0      0    0        0
#> Cote D'ivoire                                  1       0      0    0        0
#> Croatia                                        0       3      1    0        0
#> Cuba                                           0       0      0    0        0
#> Cyprus                                         0       0      0    0        1
#> Denmark                                        0       0      0    0        2
#> Djibouti                                       0       0      0    0        0
#> Dominican Republic                             0       0      0    0        1
#> Ecuador                                        0       1      0    0        0
#> Egypt                                          0       1      0    0        0
#> El Salvador                                    0       0      0    0        0
#> Equatorial Guinea                              0       1      0    0        0
#> Estonia                                        0       0      0    0        0
#> Ethiopia                                       0       2      0    0        0
#> Fiji                                           0       0      0    0        0
#> Finland                                        0       0      0    0        0
#> France                                        17       7      0    0        6
#> Gabon                                          0       0      0    0        0
#> Gambia                                         0       3      0    0        0
#> Georgia                                        0       0      0    0        1
#> Germany                                        1      10      0    0       19
#> Ghana                                          1      13      0    0        0
#> Greece                                         1       0      0    0        2
#> Guatemala                                      0       1      0    0        0
#> Guinea                                         1       1      0    0        0
#> Guinea-Bissau                                  0       1      0    0        0
#> Guyana                                         0       0      0    0        0
#> Haiti                                          0       0      0    0        0
#> Honduras                                       0       0      0    0        0
#> Hungary                                        0       0      0    0        0
#> India                                          0       4      1    0      208
#> Indonesia                                      1       5      2    0        1
#> Iran, Islamic Republic Of                      0      11      5    1       25
#> Iraq                                           0       0      2    0        1
#> Ireland                                        0       0      0    0        0
#> Israel                                         0       0      2    0        5
#> Italy                                          0       2      0    0        1
#> Jamaica                                        0       3      0    0        0
#> Japan                                          0       2      0    0        1
#> Jordan                                         0       0      0    0        3
#> Kazakhstan                                     0       0      0    0        0
#> Kenya                                          0       3      0    0        5
#> Korea, Democratic People's Republic Of         0       0      0    0        0
#> Korea, Republic Of                             0       0      0    0        4
#> Kuwait                                         0       0      0    0        0
#> Kyrgyzstan                                     0       0      0    0        2
#> Lao People's Democratic Republic               0       0      0    0        0
#> Latvia                                         0       0      0    0        0
#> Lebanon                                        0      17      0    0        0
#> Liberia                                        1       1      0    0        0
#> Libyan Arab Jamahiriya                         8      18      0    0        1
#> Lithuania                                      0       1      3    0        0
#> Luxembourg                                     0       0      0    0        0
#> Macedonia, The Former Yugoslav Republic Of     0       0      2    0        0
#> Madagascar                                     2       0      0    0        0
#> Malawi                                         1       0      0    0        0
#> Malaysia                                       0       6      0    0        1
#> Mali                                           2       0      0    0        0
#> Mauritania                                     0       0      0    0        0
#> Mauritius                                      0       0      0    0        0
#> Mexico                                         0       0      0    0        1
#> Moldova, Republic Of                           0       0      0    0        0
#> Mongolia                                       0       0      0    0        1
#> Morocco                                        1       0      0    0        0
#> Mozambique                                     0       0      0    0        2
#> Myanmar                                        0       0      0    0        0
#> Nepal                                          0       0      2    0        2
#> Netherlands                                    0       1      0    0        0
#> New Zealand                                    0       0      0    0        0
#> Nicaragua                                      0       0      1    0        0
#> Niger                                         NA      13      0    0        0
#> Nigeria                                       13      NA      0    0        0
#> Norway                                         0       0     NA    0        1
#> Oman                                           0       0      0   NA        0
#> Pakistan                                       0       0      1    0       NA
#> Panama                                         0       0      0    0        0
#> Papua New Guinea                               0       0      0    0        0
#> Paraguay                                       0       0      0    0        0
#> Peru                                           0       0      0    0        0
#> Philippines                                    0       2      1    0        0
#> Poland                                         0       3      0    0        0
#> Portugal                                       0       0      0    0        0
#> Qatar                                          0       0      0    0        0
#> Romania                                        0       0      0    0        0
#> Russian Federation                             0       3      2    0        8
#> Rwanda                                         0       0      1    0        0
#> Saudi Arabia                                   0       2      0    0        0
#> Senegal                                        1       1      0    0        0
#> Sierra Leone                                   0       0      0    0        4
#> Singapore                                      0       0      0    1        0
#> Slovakia                                       0       0      0    0        0
#> Slovenia                                       0       0      0    0        0
#> Solomon Islands                                0       0      0    0        0
#> Somalia                                        0       3      0    1        0
#> South Africa                                   0     106      0    0        6
#> Spain                                          0       3      0    0       22
#> Sri Lanka                                      1       0      1    0        2
#> Sudan                                          0       2      1    0        4
#> Suriname                                       0       0      0    0        0
#> Sweden                                         0       0      0    0        1
#> Switzerland                                    0       1      0    0        2
#> Syrian Arab Republic                           0      13      0    0        1
#> Tajikistan                                     0       0      0    0        2
#> Tanzania, United Republic Of                   0       0      0    0        0
#> Thailand                                       0       5      0    0       32
#> Togo                                           1       0      0    0        0
#> Trinidad And Tobago                            0       4      0    0        4
#> Tunisia                                        0       0      0    0        0
#> Turkmenistan                                   0       0      0    0        0
#> Uganda                                         0       0      0    0        2
#> Ukraine                                        0       1      0    0        0
#> United Arab Emirates                           0       9      0    0        3
#> United Kingdom                                 0       7      0    0       21
#> United States                                  0      48      2    0      869
#> Uruguay                                        0       0      0    0        0
#> Uzbekistan                                     0       0      3    0        4
#> Vietnam                                        0       5      0    0        0
#> Yemen                                          2       0      0    3       11
#> Zambia                                         0       1      0    0        3
#> Zimbabwe                                       0       1      0    0       17
#>                                            Panama Papua New Guinea Paraguay
#> Afghanistan                                     0                2        0
#> Albania                                         0                0        0
#> Algeria                                         0                0        0
#> Angola                                          0                0        0
#> Argentina                                       0                0        3
#> Armenia                                         0                0        0
#> Australia                                       0                6        0
#> Austria                                         0                0        0
#> Azerbaijan                                      0                0        0
#> Bahrain                                         0                0        0
#> Bangladesh                                      0                0        0
#> Belarus                                         0                0        0
#> Belgium                                         0                0        0
#> Benin                                           0                0        0
#> Bolivia, Plurinational State Of                 0                0        3
#> Bosnia And Herzegovina                          0                0        0
#> Brazil                                          0                0        7
#> Bulgaria                                        0                0        0
#> Burkina Faso                                    0                0        0
#> Burundi                                         0                0        0
#> Cambodia                                        0                0        0
#> Cameroon                                        0                0        0
#> Canada                                          0                0        0
#> Cape Verde                                      0                0        0
#> Central African Republic                        0                0        0
#> Chad                                            0                0        0
#> Chile                                           0                0        1
#> China                                           0                4        0
#> Colombia                                       15                0        1
#> Comoros                                         0                0        0
#> Congo, Republic Of                              0                0        0
#> Congo, The Democratic Republic Of               0                0        0
#> Costa Rica                                      2                0        0
#> Cote D'ivoire                                   0                0        0
#> Croatia                                         0                0        0
#> Cuba                                            1                0        0
#> Cyprus                                          0                0        0
#> Denmark                                         0                0        0
#> Djibouti                                        0                0        0
#> Dominican Republic                              1                0        0
#> Ecuador                                         0                0        1
#> Egypt                                           0                0        0
#> El Salvador                                     0                0        0
#> Equatorial Guinea                               0                0        0
#> Estonia                                         0                0        0
#> Ethiopia                                        0                0        0
#> Fiji                                            0                0        0
#> Finland                                         0                0        0
#> France                                         82                0        0
#> Gabon                                           0                0        0
#> Gambia                                          0                0        0
#> Georgia                                         0                0        0
#> Germany                                         0                0        1
#> Ghana                                           0                0        0
#> Greece                                          0                0        0
#> Guatemala                                       0                0        0
#> Guinea                                          0                0        0
#> Guinea-Bissau                                   0                0        0
#> Guyana                                          0                0        0
#> Haiti                                           0                0        0
#> Honduras                                        1                0        0
#> Hungary                                         0                0        0
#> India                                           0                0        0
#> Indonesia                                       0                1        0
#> Iran, Islamic Republic Of                       0                0        0
#> Iraq                                            0                0        0
#> Ireland                                         0                0        0
#> Israel                                          0                0        0
#> Italy                                           0                0        4
#> Jamaica                                         0                0        0
#> Japan                                           0                0        0
#> Jordan                                          0                0        0
#> Kazakhstan                                      0                0        0
#> Kenya                                           0                0        0
#> Korea, Democratic People's Republic Of          0                0        0
#> Korea, Republic Of                              0                0        0
#> Kuwait                                          0                0        0
#> Kyrgyzstan                                      0                0        0
#> Lao People's Democratic Republic                0                0        0
#> Latvia                                          0                0        0
#> Lebanon                                         0                0        2
#> Liberia                                         0                0        0
#> Libyan Arab Jamahiriya                          0                0        0
#> Lithuania                                       0                0        0
#> Luxembourg                                      0                0        0
#> Macedonia, The Former Yugoslav Republic Of      0                0        0
#> Madagascar                                      0                0        0
#> Malawi                                          0                0        0
#> Malaysia                                        0                0        0
#> Mali                                            0                0        0
#> Mauritania                                      0                0        0
#> Mauritius                                       0                0        0
#> Mexico                                          2                1        1
#> Moldova, Republic Of                            0                0        0
#> Mongolia                                        0                0        0
#> Morocco                                         0                0        0
#> Mozambique                                      0                0        0
#> Myanmar                                         0                0        0
#> Nepal                                           0                0        0
#> Netherlands                                     1                0        0
#> New Zealand                                     0                0        0
#> Nicaragua                                       0                0        0
#> Niger                                           0                0        0
#> Nigeria                                         0                0        0
#> Norway                                          0                0        0
#> Oman                                            0                0        0
#> Pakistan                                        0                0        0
#> Panama                                         NA                0        0
#> Papua New Guinea                                0               NA        0
#> Paraguay                                        0                0       NA
#> Peru                                            0                0        0
#> Philippines                                     1                0        0
#> Poland                                          0                0        0
#> Portugal                                        0                0        0
#> Qatar                                           0                0        0
#> Romania                                         0                0        0
#> Russian Federation                              1                0        0
#> Rwanda                                          0                0        0
#> Saudi Arabia                                    0                0        0
#> Senegal                                         0                0        0
#> Sierra Leone                                    0                0        0
#> Singapore                                       0                0        0
#> Slovakia                                        0                0        0
#> Slovenia                                        0                0        0
#> Solomon Islands                                 0                0        0
#> Somalia                                         2                0        0
#> South Africa                                    0                0        0
#> Spain                                           1                0        1
#> Sri Lanka                                       0                0        0
#> Sudan                                           0                0        0
#> Suriname                                        0                0        0
#> Sweden                                          0                0        0
#> Switzerland                                     0                0        1
#> Syrian Arab Republic                            0                0        0
#> Tajikistan                                      0                0        0
#> Tanzania, United Republic Of                    0                0        0
#> Thailand                                        0                0        0
#> Togo                                            0                0        0
#> Trinidad And Tobago                             0                0        0
#> Tunisia                                         0                0        0
#> Turkmenistan                                    0                0        0
#> Uganda                                          0                0        0
#> Ukraine                                         0                0        0
#> United Arab Emirates                            0                0        0
#> United Kingdom                                  1                2        0
#> United States                                  65                7        2
#> Uruguay                                         0                0        0
#> Uzbekistan                                      0                0        0
#> Vietnam                                         0                0        0
#> Yemen                                           0                0        0
#> Zambia                                          0                0        0
#> Zimbabwe                                        0                0        0
#>                                            Peru Philippines Poland Portugal
#> Afghanistan                                   0           3     23        2
#> Albania                                       0           0      0        2
#> Algeria                                       0           0      0        0
#> Angola                                        0           0      0        0
#> Argentina                                     1           1      0        1
#> Armenia                                       0           0      0        0
#> Australia                                     1          12      0        0
#> Austria                                       0           0      0        0
#> Azerbaijan                                    0           0      0        0
#> Bahrain                                       0           0      0        0
#> Bangladesh                                    0           0      0        0
#> Belarus                                       0           0     11        0
#> Belgium                                       1           0      2        0
#> Benin                                         0           0      0        0
#> Bolivia, Plurinational State Of               6           0      0        0
#> Bosnia And Herzegovina                        0           0      0        0
#> Brazil                                        5           1      2        1
#> Bulgaria                                      0           0      0        0
#> Burkina Faso                                  0           0      0        0
#> Burundi                                       0           0      0        0
#> Cambodia                                      0           1      0        0
#> Cameroon                                      0           1      0        0
#> Canada                                        0           0      9        0
#> Cape Verde                                    0           0      0        0
#> Central African Republic                      0           0      0        0
#> Chad                                          0           0      0        0
#> Chile                                         8           8      0        0
#> China                                         1          27      1        0
#> Colombia                                      2           1      0        0
#> Comoros                                       0           0      0        0
#> Congo, Republic Of                            0           0      0        0
#> Congo, The Democratic Republic Of             0           0      0        0
#> Costa Rica                                    0           0      0        0
#> Cote D'ivoire                                 0           0      0        1
#> Croatia                                       0           1      0        0
#> Cuba                                          0           0      0        0
#> Cyprus                                        0           0      0        0
#> Denmark                                       0           0      0        0
#> Djibouti                                      0           0      0        0
#> Dominican Republic                            0           0      0        0
#> Ecuador                                       2           0      0        0
#> Egypt                                         1           0      0        0
#> El Salvador                                   0           0      0        0
#> Equatorial Guinea                             0           0      0        0
#> Estonia                                       0           0      0        0
#> Ethiopia                                      0           0      0        0
#> Fiji                                          0           0      0        0
#> Finland                                       0           0      0        0
#> France                                        0           0      1        1
#> Gabon                                         0           0      0        0
#> Gambia                                        0           0      0        0
#> Georgia                                       0           3     11        0
#> Germany                                       0           4      3        0
#> Ghana                                         0           0      0        0
#> Greece                                        0           0      2        0
#> Guatemala                                     0           0      0        0
#> Guinea                                        0           0      0        0
#> Guinea-Bissau                                 0           0      0        1
#> Guyana                                        0           2      0        0
#> Haiti                                         1           0      0        0
#> Honduras                                      0           0      0        0
#> Hungary                                       0           0      2        0
#> India                                         2           4      1        3
#> Indonesia                                     0           6      0        0
#> Iran, Islamic Republic Of                     0           0      0        0
#> Iraq                                          0           0      5        0
#> Ireland                                       0           0      1        0
#> Israel                                        1           3     49        0
#> Italy                                         0           0      0        0
#> Jamaica                                       0           0      0        0
#> Japan                                         0           9      1        0
#> Jordan                                        1           0      0        0
#> Kazakhstan                                    0           0      0        0
#> Kenya                                         0           0      0        0
#> Korea, Democratic People's Republic Of        0           0      0        1
#> Korea, Republic Of                            0           6      4        0
#> Kuwait                                        0           0      0        0
#> Kyrgyzstan                                    0           0      0        0
#> Lao People's Democratic Republic              0           0      0        0
#> Latvia                                        0           0      0        0
#> Lebanon                                       2           0      2        0
#> Liberia                                       0           0      0        0
#> Libyan Arab Jamahiriya                        0           0      0        0
#> Lithuania                                     0           0      3        0
#> Luxembourg                                    0           0      0        0
#> Macedonia, The Former Yugoslav Republic Of    0           0      1        1
#> Madagascar                                    0           0      0        0
#> Malawi                                        0           0      0        0
#> Malaysia                                      0          10      0        0
#> Mali                                          0           0      0        0
#> Mauritania                                    0           0      0        0
#> Mauritius                                     0           0      0        0
#> Mexico                                        2           1      0        0
#> Moldova, Republic Of                          0           0      0        0
#> Mongolia                                      0           0      0        0
#> Morocco                                       0           0      0        0
#> Mozambique                                    0           0      0        0
#> Myanmar                                       0           0      0        0
#> Nepal                                         0           0      0        0
#> Netherlands                                   2           0      1        0
#> New Zealand                                   0           3      0        0
#> Nicaragua                                     0           0      0        0
#> Niger                                         0           0      0        0
#> Nigeria                                       0           2      3        0
#> Norway                                        0           1      0        0
#> Oman                                          0           0      0        0
#> Pakistan                                      0           0      0        0
#> Panama                                        0           1      0        0
#> Papua New Guinea                              0           0      0        0
#> Paraguay                                      0           0      0        0
#> Peru                                         NA           2      1        0
#> Philippines                                   2          NA      0        0
#> Poland                                        1           0     NA        0
#> Portugal                                      0           0      0       NA
#> Qatar                                         0           1      0        0
#> Romania                                       0           0      0        2
#> Russian Federation                            1           1      8        0
#> Rwanda                                        0           0      0        0
#> Saudi Arabia                                  0           4      0        0
#> Senegal                                       0           0      0        0
#> Sierra Leone                                  0           0      0        0
#> Singapore                                     0           3      0        0
#> Slovakia                                      2           0      2        0
#> Slovenia                                      0           0      0        0
#> Solomon Islands                               0           0      0        0
#> Somalia                                       0           4      0        0
#> South Africa                                  0           0      0        0
#> Spain                                         1           4      1        3
#> Sri Lanka                                     1           2      0        0
#> Sudan                                         0           0      0        0
#> Suriname                                      0           0      0        0
#> Sweden                                        0           0      8        0
#> Switzerland                                   0           1      0        0
#> Syrian Arab Republic                          0           0      0        0
#> Tajikistan                                    0           0      0        0
#> Tanzania, United Republic Of                  0           0      0        0
#> Thailand                                      0          15      1        0
#> Togo                                          0           0      0        0
#> Trinidad And Tobago                           0           0      0        0
#> Tunisia                                       0           0      0        0
#> Turkmenistan                                  0           0      0        0
#> Uganda                                        0           0      0        0
#> Ukraine                                       0           0      3        1
#> United Arab Emirates                          0           0      0        0
#> United Kingdom                                2           1      7        9
#> United States                                14          29     36        1
#> Uruguay                                       0           0      0        0
#> Uzbekistan                                    0           0      0        0
#> Vietnam                                       0           1      1        0
#> Yemen                                         0           3      0        0
#> Zambia                                        0           0      0        0
#> Zimbabwe                                      0           0      0        0
#>                                            Qatar Romania Russian Federation
#> Afghanistan                                    1      15                 27
#> Albania                                        0       0                  0
#> Algeria                                        0       0                  2
#> Angola                                         0       0                  0
#> Argentina                                      0       0                  2
#> Armenia                                        0       0                 13
#> Australia                                      0       1                  0
#> Austria                                        0       0                  9
#> Azerbaijan                                     0       0                 22
#> Bahrain                                       24       0                  0
#> Bangladesh                                     2       1                 13
#> Belarus                                        0       0                 76
#> Belgium                                        0       1                  9
#> Benin                                          0       0                  0
#> Bolivia, Plurinational State Of                0       0                  0
#> Bosnia And Herzegovina                         0       2                  0
#> Brazil                                         0       0                  1
#> Bulgaria                                       0       1                  2
#> Burkina Faso                                   0       0                  0
#> Burundi                                        0       0                  0
#> Cambodia                                       0       0                  1
#> Cameroon                                       0       0                  5
#> Canada                                         0       0                  1
#> Cape Verde                                     0       0                  0
#> Central African Republic                       0       0                  2
#> Chad                                           0       0                  0
#> Chile                                          0       0                  0
#> China                                          0       3                 19
#> Colombia                                       0       0                  1
#> Comoros                                        0       0                  0
#> Congo, Republic Of                             0       0                  0
#> Congo, The Democratic Republic Of              0       0                  0
#> Costa Rica                                     0       0                  0
#> Cote D'ivoire                                  0       0                  0
#> Croatia                                        0       1                  0
#> Cuba                                           0       0                  0
#> Cyprus                                         0       3                  7
#> Denmark                                        0       0                  1
#> Djibouti                                       0       0                  0
#> Dominican Republic                             0       0                  0
#> Ecuador                                        0       0                  1
#> Egypt                                          3       2                  1
#> El Salvador                                    0       0                  0
#> Equatorial Guinea                              0       0                  0
#> Estonia                                        0       0                  6
#> Ethiopia                                       0       0                  0
#> Fiji                                           0       0                  0
#> Finland                                        0       0                  3
#> France                                         0       1                  9
#> Gabon                                          0       0                  0
#> Gambia                                         0       0                  0
#> Georgia                                        0       0                172
#> Germany                                        0       1                 10
#> Ghana                                          0       0                  1
#> Greece                                         0       8                  5
#> Guatemala                                      0       0                  0
#> Guinea                                         0       0                  0
#> Guinea-Bissau                                  0       0                  0
#> Guyana                                         0       0                  4
#> Haiti                                          0       1                  2
#> Honduras                                       0       0                  0
#> Hungary                                        0       5                  5
#> India                                          1       0                  8
#> Indonesia                                      0       0                  1
#> Iran, Islamic Republic Of                      3       1                108
#> Iraq                                           1       1                  4
#> Ireland                                        0       0                  0
#> Israel                                         3       2                 11
#> Italy                                          0       0                  1
#> Jamaica                                        0       0                  0
#> Japan                                          0       0                 16
#> Jordan                                         0       0                  1
#> Kazakhstan                                     0       0                 13
#> Kenya                                          0       0                  2
#> Korea, Democratic People's Republic Of         0       0                 20
#> Korea, Republic Of                             0       0                 34
#> Kuwait                                         4       0                  0
#> Kyrgyzstan                                     0       0                 52
#> Lao People's Democratic Republic               0       0                  0
#> Latvia                                         0       0                  6
#> Lebanon                                        1       0                  0
#> Liberia                                        0       0                 19
#> Libyan Arab Jamahiriya                         1       0                  0
#> Lithuania                                      0       0                  1
#> Luxembourg                                     0       0                  0
#> Macedonia, The Former Yugoslav Republic Of     0       1                  0
#> Madagascar                                     0       0                  0
#> Malawi                                         0       0                  0
#> Malaysia                                       0       0                  2
#> Mali                                           0       0                  0
#> Mauritania                                     1       0                  0
#> Mauritius                                      0       0                  0
#> Mexico                                         0       0                  1
#> Moldova, Republic Of                           0       1                 11
#> Mongolia                                       0       0                  2
#> Morocco                                        3       0                  0
#> Mozambique                                     0       0                  0
#> Myanmar                                        0       0                  0
#> Nepal                                          0       0                  0
#> Netherlands                                    0       0                  1
#> New Zealand                                    0       0                  0
#> Nicaragua                                      0       0                  0
#> Niger                                          0       0                  0
#> Nigeria                                        0       0                  3
#> Norway                                         0       0                  2
#> Oman                                           0       0                  0
#> Pakistan                                       0       0                  8
#> Panama                                         0       0                  1
#> Papua New Guinea                               0       0                  0
#> Paraguay                                       0       0                  0
#> Peru                                           0       0                  1
#> Philippines                                    1       0                  1
#> Poland                                         0       0                  8
#> Portugal                                       0       2                  0
#> Qatar                                         NA       0                  0
#> Romania                                        0      NA                 24
#> Russian Federation                             0      24                 NA
#> Rwanda                                         0       1                  0
#> Saudi Arabia                                   1       0                  0
#> Senegal                                        0       1                  0
#> Sierra Leone                                   0       0                  2
#> Singapore                                      0       2                  0
#> Slovakia                                       0       0                  1
#> Slovenia                                       0       0                  2
#> Solomon Islands                                0       0                  0
#> Somalia                                        0       0                 13
#> South Africa                                   1       0                  0
#> Spain                                          0      13                  9
#> Sri Lanka                                      0       0                  1
#> Sudan                                          7       0                 13
#> Suriname                                       0       0                  2
#> Sweden                                         0       0                  1
#> Switzerland                                    0       0                  4
#> Syrian Arab Republic                           1       0                  1
#> Tajikistan                                     0       0                  7
#> Tanzania, United Republic Of                   0       0                  0
#> Thailand                                       0       0                107
#> Togo                                           0       0                  0
#> Trinidad And Tobago                            0       0                  4
#> Tunisia                                        0       0                  0
#> Turkmenistan                                   0       0                  1
#> Uganda                                         0       0                  0
#> Ukraine                                        0       1                 29
#> United Arab Emirates                           0       0                  6
#> United Kingdom                                 0       2                 50
#> United States                                  1       6                388
#> Uruguay                                        0       0                  1
#> Uzbekistan                                     0       0                  7
#> Vietnam                                        0       0                  4
#> Yemen                                          0       0                  0
#> Zambia                                         0       0                  0
#> Zimbabwe                                       0       0                  0
#>                                            Rwanda Saudi Arabia Senegal
#> Afghanistan                                     0            4       0
#> Albania                                         0            0       0
#> Algeria                                         0            0       1
#> Angola                                          0            0       0
#> Argentina                                       0            0       0
#> Armenia                                         0            0       0
#> Australia                                       0            0       0
#> Austria                                         0            0       0
#> Azerbaijan                                      0            0       0
#> Bahrain                                         0            0       0
#> Bangladesh                                      2            1       0
#> Belarus                                         0            0       0
#> Belgium                                         2            0       0
#> Benin                                           0            0       0
#> Bolivia, Plurinational State Of                 0            0       0
#> Bosnia And Herzegovina                          0            0       0
#> Brazil                                          0            0       0
#> Bulgaria                                        0            0       0
#> Burkina Faso                                    0            0       2
#> Burundi                                         1            0       0
#> Cambodia                                        0            0       0
#> Cameroon                                        0            0       0
#> Canada                                          2            2       0
#> Cape Verde                                      0            0       0
#> Central African Republic                        0            0       0
#> Chad                                            0            0       4
#> Chile                                           0            0       0
#> China                                           0            3       0
#> Colombia                                        0            0       0
#> Comoros                                         0            0       0
#> Congo, Republic Of                             24            0       0
#> Congo, The Democratic Republic Of              31            0       0
#> Costa Rica                                      0            0       0
#> Cote D'ivoire                                   0            0       1
#> Croatia                                         0            0       0
#> Cuba                                            0            2       0
#> Cyprus                                          0            0       0
#> Denmark                                         0            2       0
#> Djibouti                                        0            0       0
#> Dominican Republic                              0            0       0
#> Ecuador                                         0            0       0
#> Egypt                                           0            3       1
#> El Salvador                                     0            0       0
#> Equatorial Guinea                               0            0       0
#> Estonia                                         0            0       0
#> Ethiopia                                        3            2       0
#> Fiji                                            0            0       0
#> Finland                                         6            0       0
#> France                                          8            5       0
#> Gabon                                           2            0       0
#> Gambia                                          0            0       0
#> Georgia                                         0            0       0
#> Germany                                         7            0       0
#> Ghana                                           0            0       0
#> Greece                                          0            0       0
#> Guatemala                                       0            0       0
#> Guinea                                          0            0       1
#> Guinea-Bissau                                   0            0       2
#> Guyana                                          0            0       0
#> Haiti                                           0            0       0
#> Honduras                                        0            0       0
#> Hungary                                         0            0       1
#> India                                           1            1       0
#> Indonesia                                       0           21       0
#> Iran, Islamic Republic Of                       0            4      13
#> Iraq                                            0            5       0
#> Ireland                                         0            0       0
#> Israel                                          0            6       3
#> Italy                                           0            0       0
#> Jamaica                                         0            6       0
#> Japan                                           0            0       0
#> Jordan                                          0            3       0
#> Kazakhstan                                      0            0       0
#> Kenya                                           0           18       0
#> Korea, Democratic People's Republic Of          0            0       0
#> Korea, Republic Of                              0            0       0
#> Kuwait                                          0            0       0
#> Kyrgyzstan                                      0            0       0
#> Lao People's Democratic Republic                0            0       0
#> Latvia                                          0            0       0
#> Lebanon                                         0            8       0
#> Liberia                                         0            0       0
#> Libyan Arab Jamahiriya                          0            0       0
#> Lithuania                                       0            0       0
#> Luxembourg                                      0            0       0
#> Macedonia, The Former Yugoslav Republic Of      0            0       0
#> Madagascar                                      0            0       0
#> Malawi                                          0            0       0
#> Malaysia                                        0            1       0
#> Mali                                            0            0       1
#> Mauritania                                      0            0       1
#> Mauritius                                       0            0       0
#> Mexico                                          0            0       0
#> Moldova, Republic Of                            0            0       0
#> Mongolia                                        0            0       0
#> Morocco                                         0            0       1
#> Mozambique                                      0            0       0
#> Myanmar                                         0            0       0
#> Nepal                                           0            0       0
#> Netherlands                                     4            1       0
#> New Zealand                                     0            0       0
#> Nicaragua                                       0            0       0
#> Niger                                           0            0       1
#> Nigeria                                         0            2       1
#> Norway                                          1            0       0
#> Oman                                            0            0       0
#> Pakistan                                        0            0       0
#> Panama                                          0            0       0
#> Papua New Guinea                                0            0       0
#> Paraguay                                        0            0       0
#> Peru                                            0            0       0
#> Philippines                                     0            4       0
#> Poland                                          0            0       0
#> Portugal                                        0            0       0
#> Qatar                                           0            1       0
#> Romania                                         1            0       1
#> Russian Federation                              0            0       0
#> Rwanda                                         NA            0       0
#> Saudi Arabia                                    0           NA       0
#> Senegal                                         0            0      NA
#> Sierra Leone                                    0            0       0
#> Singapore                                       0            0       0
#> Slovakia                                        0            0       0
#> Slovenia                                        0            0       0
#> Solomon Islands                                 0            0       0
#> Somalia                                         0           19       0
#> South Africa                                   15            1       0
#> Spain                                           2            0       0
#> Sri Lanka                                       0            3       0
#> Sudan                                           5            0       1
#> Suriname                                        0            0       0
#> Sweden                                          0            0       1
#> Switzerland                                     0            0       1
#> Syrian Arab Republic                            0            2       0
#> Tajikistan                                      0            0       0
#> Tanzania, United Republic Of                    1            0       0
#> Thailand                                        0            4       0
#> Togo                                            0            0       0
#> Trinidad And Tobago                             0            0       0
#> Tunisia                                         0            0       0
#> Turkmenistan                                    0            0       0
#> Uganda                                         30            0       1
#> Ukraine                                         0            0       0
#> United Arab Emirates                            0            2       0
#> United Kingdom                                  0           19       0
#> United States                                   6            9       1
#> Uruguay                                         0            0       0
#> Uzbekistan                                      0            0       0
#> Vietnam                                         0            0       0
#> Yemen                                           0           30       0
#> Zambia                                          0            0       0
#> Zimbabwe                                        0            0       0
#>                                            Sierra Leone Singapore Slovakia
#> Afghanistan                                           0         2       18
#> Albania                                               0         0        1
#> Algeria                                               0         0        2
#> Angola                                                0         0        0
#> Argentina                                             0         0        0
#> Armenia                                               0         0        0
#> Australia                                             0         6        0
#> Austria                                               0         0        4
#> Azerbaijan                                            0         0        0
#> Bahrain                                               0         0        0
#> Bangladesh                                            1         1        0
#> Belarus                                               0         0        0
#> Belgium                                               0         2        0
#> Benin                                                 0         0        0
#> Bolivia, Plurinational State Of                       0         0        0
#> Bosnia And Herzegovina                                0         0        0
#> Brazil                                                0         0        0
#> Bulgaria                                              0         0        0
#> Burkina Faso                                          0         0        0
#> Burundi                                               0         0        0
#> Cambodia                                              0         0        0
#> Cameroon                                              0         0        0
#> Canada                                                0         0        0
#> Cape Verde                                            0         0        0
#> Central African Republic                              0         0       10
#> Chad                                                  0         0        0
#> Chile                                                 0         0        0
#> China                                                 2         0        0
#> Colombia                                              0         0        0
#> Comoros                                               0         0        0
#> Congo, Republic Of                                    0         0        0
#> Congo, The Democratic Republic Of                     0         0        0
#> Costa Rica                                            0         0        0
#> Cote D'ivoire                                         0         0        0
#> Croatia                                               0         0        0
#> Cuba                                                  0         0        0
#> Cyprus                                                0         0        0
#> Denmark                                               0         0        0
#> Djibouti                                              0         0        0
#> Dominican Republic                                    0         0        0
#> Ecuador                                               0         0        0
#> Egypt                                                 0         0        1
#> El Salvador                                           0         0        0
#> Equatorial Guinea                                     0         0        0
#> Estonia                                               0         0        0
#> Ethiopia                                              0         0        0
#> Fiji                                                  0         0        0
#> Finland                                               0         0        0
#> France                                                0         0        0
#> Gabon                                                 0         0        0
#> Gambia                                                0         0        0
#> Georgia                                               0         0        2
#> Germany                                               0         0        2
#> Ghana                                                 0         0        0
#> Greece                                                0         0        1
#> Guatemala                                             0         0        0
#> Guinea                                                1         0        0
#> Guinea-Bissau                                         0         0        0
#> Guyana                                                0         0        0
#> Haiti                                                 0         0        0
#> Honduras                                              0         0        0
#> Hungary                                               0         0       11
#> India                                                 0         5        0
#> Indonesia                                             0         2        0
#> Iran, Islamic Republic Of                             0         0        0
#> Iraq                                                  0         0        0
#> Ireland                                               0         0        7
#> Israel                                                0         0        1
#> Italy                                                 0         0        0
#> Jamaica                                               0         0        0
#> Japan                                                 0         2        0
#> Jordan                                                0         0        0
#> Kazakhstan                                            0         0        1
#> Kenya                                                 0         1        0
#> Korea, Democratic People's Republic Of                0         0        0
#> Korea, Republic Of                                    0         0        0
#> Kuwait                                                0         0        0
#> Kyrgyzstan                                            2         0        0
#> Lao People's Democratic Republic                      0         0        0
#> Latvia                                                0         0        0
#> Lebanon                                               0         0        0
#> Liberia                                               3         0        0
#> Libyan Arab Jamahiriya                                0         0        0
#> Lithuania                                             0         0        1
#> Luxembourg                                            0         0        0
#> Macedonia, The Former Yugoslav Republic Of            0         0        1
#> Madagascar                                            0         0        0
#> Malawi                                                0         0        0
#> Malaysia                                              0        10        0
#> Mali                                                  0         0        0
#> Mauritania                                            0         0        0
#> Mauritius                                             0         0        0
#> Mexico                                                0         1        0
#> Moldova, Republic Of                                  0         0        0
#> Mongolia                                              1         0        0
#> Morocco                                               0         0        0
#> Mozambique                                            0         0        0
#> Myanmar                                               0         0        0
#> Nepal                                                 0         0        0
#> Netherlands                                           0         0        2
#> New Zealand                                           0         0        0
#> Nicaragua                                             0         0        0
#> Niger                                                 0         0        0
#> Nigeria                                               0         0        0
#> Norway                                                0         0        0
#> Oman                                                  0         1        0
#> Pakistan                                              4         0        0
#> Panama                                                0         0        0
#> Papua New Guinea                                      0         0        0
#> Paraguay                                              0         0        0
#> Peru                                                  0         0        2
#> Philippines                                           0         3        0
#> Poland                                                0         0        2
#> Portugal                                              0         0        0
#> Qatar                                                 0         0        0
#> Romania                                               0         2        0
#> Russian Federation                                    2         0        1
#> Rwanda                                                0         0        0
#> Saudi Arabia                                          0         0        0
#> Senegal                                               0         0        0
#> Sierra Leone                                         NA         0        0
#> Singapore                                             0        NA        0
#> Slovakia                                              0         0       NA
#> Slovenia                                              0         0        0
#> Solomon Islands                                       0         0        0
#> Somalia                                               0         1        0
#> South Africa                                          0         0        0
#> Spain                                                 0         0        0
#> Sri Lanka                                             0         0        0
#> Sudan                                                 1         0        0
#> Suriname                                              0         0        0
#> Sweden                                                0         0        1
#> Switzerland                                           0         6        0
#> Syrian Arab Republic                                  0         0        0
#> Tajikistan                                            0         0        0
#> Tanzania, United Republic Of                          0         1        0
#> Thailand                                              0         3        2
#> Togo                                                  0         0        0
#> Trinidad And Tobago                                   0         0        0
#> Tunisia                                               0         0        0
#> Turkmenistan                                          0         0        0
#> Uganda                                                0         0        0
#> Ukraine                                               0         0        2
#> United Arab Emirates                                  0         0        0
#> United Kingdom                                        0        31        0
#> United States                                         1         4        0
#> Uruguay                                               0         0        0
#> Uzbekistan                                            0         0        0
#> Vietnam                                               0         2        2
#> Yemen                                                 0         0        0
#> Zambia                                                0         0        0
#> Zimbabwe                                              0         0        0
#>                                            Slovenia Solomon Islands Somalia
#> Afghanistan                                       2               0       0
#> Albania                                           0               0       0
#> Algeria                                           1               0       2
#> Angola                                            0               0       3
#> Argentina                                         0               0       0
#> Armenia                                           0               0       1
#> Australia                                         0               3       2
#> Austria                                           0               0       0
#> Azerbaijan                                        0               0       0
#> Bahrain                                           0               0       0
#> Bangladesh                                        0               0       0
#> Belarus                                           0               0       0
#> Belgium                                           0               0       0
#> Benin                                             0               0       0
#> Bolivia, Plurinational State Of                   0               0       0
#> Bosnia And Herzegovina                            4               0       0
#> Brazil                                            0               0       0
#> Bulgaria                                          0               0       1
#> Burkina Faso                                      0               0       0
#> Burundi                                           0               0       8
#> Cambodia                                          0               0       0
#> Cameroon                                          0               0       0
#> Canada                                            0               0       2
#> Cape Verde                                        0               0       0
#> Central African Republic                          0               0       0
#> Chad                                              0               0       0
#> Chile                                             0               0       0
#> China                                             0               0       2
#> Colombia                                          0               0       1
#> Comoros                                           0               0       0
#> Congo, Republic Of                                0               0       0
#> Congo, The Democratic Republic Of                 0               0       0
#> Costa Rica                                        0               0       1
#> Cote D'ivoire                                     0               0       2
#> Croatia                                           3               0       1
#> Cuba                                              0               0       0
#> Cyprus                                            0               0       0
#> Denmark                                           1               0      17
#> Djibouti                                          0               0       1
#> Dominican Republic                                0               0       0
#> Ecuador                                           0               0       0
#> Egypt                                             0               0       5
#> El Salvador                                       0               0       0
#> Equatorial Guinea                                 0               0       0
#> Estonia                                           0               0       0
#> Ethiopia                                          0               0      41
#> Fiji                                              0               0       0
#> Finland                                           0               0       0
#> France                                            0               0       1
#> Gabon                                             0               0       0
#> Gambia                                            0               0       0
#> Georgia                                           0               0       6
#> Germany                                           0               0       7
#> Ghana                                             0               0       4
#> Greece                                            0               0       2
#> Guatemala                                         0               0       0
#> Guinea                                            0               0       3
#> Guinea-Bissau                                     0               0       0
#> Guyana                                            0               0       0
#> Haiti                                             0               0       0
#> Honduras                                          0               0       0
#> Hungary                                           0               0       0
#> India                                             0               0       5
#> Indonesia                                         0               0       1
#> Iran, Islamic Republic Of                         0               0      10
#> Iraq                                              0               0       7
#> Ireland                                           0               0       0
#> Israel                                            0               0       0
#> Italy                                             0               0       1
#> Jamaica                                           0               0       0
#> Japan                                             0               0       1
#> Jordan                                            0               0       0
#> Kazakhstan                                        0               0       0
#> Kenya                                             0               0      35
#> Korea, Democratic People's Republic Of            0               0       6
#> Korea, Republic Of                                0               0       2
#> Kuwait                                            0               0       0
#> Kyrgyzstan                                        0               0       0
#> Lao People's Democratic Republic                  0               0       0
#> Latvia                                            0               0       0
#> Lebanon                                           0               0       0
#> Liberia                                           0               0       0
#> Libyan Arab Jamahiriya                            0               0       0
#> Lithuania                                         0               0       0
#> Luxembourg                                        0               0       0
#> Macedonia, The Former Yugoslav Republic Of        0               0       0
#> Madagascar                                        0               0       0
#> Malawi                                            0               0       2
#> Malaysia                                          0               0       1
#> Mali                                              0               0       0
#> Mauritania                                        0               0       0
#> Mauritius                                         0               0       0
#> Mexico                                            0               0       0
#> Moldova, Republic Of                              0               0       0
#> Mongolia                                          0               0       0
#> Morocco                                           0               0       0
#> Mozambique                                        0               0      13
#> Myanmar                                           0               0       0
#> Nepal                                             0               0       0
#> Netherlands                                       0               0       1
#> New Zealand                                       0               2       2
#> Nicaragua                                         0               0       0
#> Niger                                             0               0       0
#> Nigeria                                           0               0       3
#> Norway                                            0               0       0
#> Oman                                              0               0       1
#> Pakistan                                          0               0       0
#> Panama                                            0               0       2
#> Papua New Guinea                                  0               0       0
#> Paraguay                                          0               0       0
#> Peru                                              0               0       0
#> Philippines                                       0               0       4
#> Poland                                            0               0       0
#> Portugal                                          0               0       0
#> Qatar                                             0               0       0
#> Romania                                           0               0       0
#> Russian Federation                                2               0      13
#> Rwanda                                            0               0       0
#> Saudi Arabia                                      0               0      19
#> Senegal                                           0               0       0
#> Sierra Leone                                      0               0       0
#> Singapore                                         0               0       1
#> Slovakia                                          0               0       0
#> Slovenia                                         NA               0       1
#> Solomon Islands                                   0              NA       0
#> Somalia                                           1               0      NA
#> South Africa                                      0               0      22
#> Spain                                             0               0       4
#> Sri Lanka                                         0               0       2
#> Sudan                                             1               0       2
#> Suriname                                          0               0       0
#> Sweden                                            0               0       4
#> Switzerland                                       0               0       4
#> Syrian Arab Republic                              0               0       0
#> Tajikistan                                        0               0       0
#> Tanzania, United Republic Of                      0               0       2
#> Thailand                                          0               0       5
#> Togo                                              0               0       0
#> Trinidad And Tobago                               0               0       0
#> Tunisia                                           0               0       0
#> Turkmenistan                                      0               0       0
#> Uganda                                            0               0      73
#> Ukraine                                           0               0       2
#> United Arab Emirates                              0               0       0
#> United Kingdom                                    0               0      10
#> United States                                     0               0      60
#> Uruguay                                           0               0       0
#> Uzbekistan                                        0               0       0
#> Vietnam                                           0               0       0
#> Yemen                                             0               0      65
#> Zambia                                            0               0       8
#> Zimbabwe                                          0               0       1
#>                                            South Africa Spain Sri Lanka Sudan
#> Afghanistan                                           0    26         0     0
#> Albania                                               0     0         0     0
#> Algeria                                               1     2         0     0
#> Angola                                                1     0         0     0
#> Argentina                                             6     6         0     0
#> Armenia                                               0     0         0     0
#> Australia                                             2     0         3     3
#> Austria                                               0     1         0     0
#> Azerbaijan                                            0     0         0     0
#> Bahrain                                               0     0         1     0
#> Bangladesh                                            1     2         0     0
#> Belarus                                               1     0         0     0
#> Belgium                                               1    15         0     1
#> Benin                                                 0     0         0     0
#> Bolivia, Plurinational State Of                       0     6         0     0
#> Bosnia And Herzegovina                                0     3         0     0
#> Brazil                                                2     5         0     0
#> Bulgaria                                              0     0         0     0
#> Burkina Faso                                          0     2         0     2
#> Burundi                                               0     0         0     0
#> Cambodia                                              0     0         0     0
#> Cameroon                                              0     0         0     0
#> Canada                                                1     2         2     0
#> Cape Verde                                            0     0         0     0
#> Central African Republic                              0     0         0     3
#> Chad                                                  0     0         0    23
#> Chile                                                 1     5         0     0
#> China                                                 7     0         1     4
#> Colombia                                              0    37         0     0
#> Comoros                                               0     0         0     0
#> Congo, Republic Of                                    1     0         0     0
#> Congo, The Democratic Republic Of                     0     3         0     2
#> Costa Rica                                            0     0         0     0
#> Cote D'ivoire                                         1     0         0     0
#> Croatia                                               0     0         0     0
#> Cuba                                                  0     2         0     0
#> Cyprus                                                0     0         0     0
#> Denmark                                               1     0         0     0
#> Djibouti                                              0     1         0     0
#> Dominican Republic                                    0     0         0     0
#> Ecuador                                               0     3         0     0
#> Egypt                                                 0     0         0     3
#> El Salvador                                           0     2         0     0
#> Equatorial Guinea                                     0     0         0     0
#> Estonia                                               0     0         0     0
#> Ethiopia                                              3     0         0     1
#> Fiji                                                  0     0         0     0
#> Finland                                               0     0         0     0
#> France                                                0    36         2     2
#> Gabon                                                 0     0         0     0
#> Gambia                                                0     3         0     0
#> Georgia                                               0    10         0     0
#> Germany                                               3     1         3    11
#> Ghana                                                 0     0         0     0
#> Greece                                                0     1         0     2
#> Guatemala                                             0    12         0     0
#> Guinea                                                1     0         0     0
#> Guinea-Bissau                                         0     0         0     0
#> Guyana                                                0     0         0     0
#> Haiti                                                 0     2         0     0
#> Honduras                                              0     0         0     0
#> Hungary                                               0     0         0     0
#> India                                                 3     3        22     4
#> Indonesia                                             1     0         1     1
#> Iran, Islamic Republic Of                             1     1         3     1
#> Iraq                                                  0     1         1     1
#> Ireland                                               0     2         0     0
#> Israel                                               26     4         0     4
#> Italy                                                 0     8         0     0
#> Jamaica                                               0     0         0     0
#> Japan                                                 0     0         0     0
#> Jordan                                                0     0         0     3
#> Kazakhstan                                            0     0         0     0
#> Kenya                                                 0     0         0     2
#> Korea, Democratic People's Republic Of                0     0         1     1
#> Korea, Republic Of                                    0     0         6     0
#> Kuwait                                                0     0         0     0
#> Kyrgyzstan                                            0     0         0     0
#> Lao People's Democratic Republic                      0     0         0     0
#> Latvia                                                0     0         0     8
#> Lebanon                                               0     8         2    10
#> Liberia                                               1     0         0     1
#> Libyan Arab Jamahiriya                                0     0         0    17
#> Lithuania                                             0     0         0     0
#> Luxembourg                                            0     0         0     0
#> Macedonia, The Former Yugoslav Republic Of            0     0         0     0
#> Madagascar                                            0     0         0     0
#> Malawi                                                2     0         0     0
#> Malaysia                                              3     0         3     0
#> Mali                                                  0     7         0     0
#> Mauritania                                            0     9         0     0
#> Mauritius                                             0     0         0     0
#> Mexico                                                0     7         0     0
#> Moldova, Republic Of                                  0     0         0     0
#> Mongolia                                              0     0         0     0
#> Morocco                                               0    21         0     0
#> Mozambique                                           11     0         0     1
#> Myanmar                                               0     0         0     0
#> Nepal                                                 0     0         0     0
#> Netherlands                                           9     2         0     3
#> New Zealand                                           0     0         0     0
#> Nicaragua                                             0     0         0     0
#> Niger                                                 0     0         1     0
#> Nigeria                                             106     3         0     2
#> Norway                                                0     0         1     1
#> Oman                                                  0     0         0     0
#> Pakistan                                              6    22         2     4
#> Panama                                                0     1         0     0
#> Papua New Guinea                                      0     0         0     0
#> Paraguay                                              0     1         0     0
#> Peru                                                  0     1         1     0
#> Philippines                                           0     4         2     0
#> Poland                                                0     1         0     0
#> Portugal                                              0     3         0     0
#> Qatar                                                 1     0         0     7
#> Romania                                               0    13         0     0
#> Russian Federation                                    0     9         1    13
#> Rwanda                                               15     2         0     5
#> Saudi Arabia                                          1     0         3     0
#> Senegal                                               0     0         0     1
#> Sierra Leone                                          0     0         0     1
#> Singapore                                             0     0         0     0
#> Slovakia                                              0     0         0     0
#> Slovenia                                              0     0         0     1
#> Solomon Islands                                       0     0         0     0
#> Somalia                                              22     4         2     2
#> South Africa                                         NA     0         0     7
#> Spain                                                 0    NA         0     2
#> Sri Lanka                                             0     0        NA     0
#> Sudan                                                 7     2         0    NA
#> Suriname                                              0     0         0     0
#> Sweden                                                1     0         0     0
#> Switzerland                                           0     1         2     1
#> Syrian Arab Republic                                  0     3         0     0
#> Tajikistan                                            0     0         0     0
#> Tanzania, United Republic Of                          0     0         0     0
#> Thailand                                              3     1        14     0
#> Togo                                                  0     0         0     0
#> Trinidad And Tobago                                   1     0         2     0
#> Tunisia                                               2     0         0     0
#> Turkmenistan                                          0     0         0     0
#> Uganda                                                5     0         0    25
#> Ukraine                                               1     0         0     0
#> United Arab Emirates                                  1     0         0     0
#> United Kingdom                                       12    20         9     3
#> United States                                         8    29         1    14
#> Uruguay                                               0     1         0     0
#> Uzbekistan                                            0     1         1     0
#> Vietnam                                               1     0         0     0
#> Yemen                                                 0     1         0     4
#> Zambia                                                2     0         0     1
#> Zimbabwe                                             13     0         0     0
#>                                            Suriname Sweden Switzerland
#> Afghanistan                                       0      3           0
#> Albania                                           0      0           0
#> Algeria                                           0      0           0
#> Angola                                            0      0           0
#> Argentina                                         0      0           0
#> Armenia                                           0      0           0
#> Australia                                         0    116           3
#> Austria                                           0      0           0
#> Azerbaijan                                        0      1           0
#> Bahrain                                           0      0           0
#> Bangladesh                                        0      0           0
#> Belarus                                           0      0           0
#> Belgium                                           0      0           0
#> Benin                                             0      0           0
#> Bolivia, Plurinational State Of                   0      0           0
#> Bosnia And Herzegovina                            0      0           0
#> Brazil                                            2      1           1
#> Bulgaria                                          0      1           0
#> Burkina Faso                                      0      0           0
#> Burundi                                           0      0           0
#> Cambodia                                          0      1           1
#> Cameroon                                          0      0           0
#> Canada                                            0      1           0
#> Cape Verde                                        0      0           0
#> Central African Republic                          0      0           0
#> Chad                                              0      0           0
#> Chile                                             0      0           1
#> China                                             0      0           1
#> Colombia                                          0      0           0
#> Comoros                                           0      0           0
#> Congo, Republic Of                                0      0           0
#> Congo, The Democratic Republic Of                 0      0           0
#> Costa Rica                                        0      0           0
#> Cote D'ivoire                                     0      0           0
#> Croatia                                           0      0           0
#> Cuba                                              0      0           0
#> Cyprus                                            0      0           0
#> Denmark                                           0      1           0
#> Djibouti                                          0      0           0
#> Dominican Republic                                0      0           0
#> Ecuador                                           0      0           0
#> Egypt                                            10      1           0
#> El Salvador                                       0      0           0
#> Equatorial Guinea                                 0      0           0
#> Estonia                                           0      0           0
#> Ethiopia                                          0      2           0
#> Fiji                                              0      0           0
#> Finland                                           0      0           0
#> France                                            0      0           1
#> Gabon                                             0      0           0
#> Gambia                                            0      0           0
#> Georgia                                           0      0           1
#> Germany                                           0      2           4
#> Ghana                                             0      0           1
#> Greece                                            0      0           0
#> Guatemala                                         0      0           0
#> Guinea                                            0      0           1
#> Guinea-Bissau                                     0      0           0
#> Guyana                                            0      0           0
#> Haiti                                             0      2           0
#> Honduras                                          0      0           0
#> Hungary                                           0      0           0
#> India                                             2      0           0
#> Indonesia                                         0      0           1
#> Iran, Islamic Republic Of                         0      5          11
#> Iraq                                              0      4           0
#> Ireland                                           0      1           0
#> Israel                                            0      1           1
#> Italy                                             0      0           1
#> Jamaica                                           0      0           0
#> Japan                                             0      0           0
#> Jordan                                            0      0           0
#> Kazakhstan                                        0      0           0
#> Kenya                                             0      0           0
#> Korea, Democratic People's Republic Of            0      0           0
#> Korea, Republic Of                                0      0           0
#> Kuwait                                            0      0           0
#> Kyrgyzstan                                        0      0           0
#> Lao People's Democratic Republic                  0      0           0
#> Latvia                                            0      2           0
#> Lebanon                                           0      1           1
#> Liberia                                           0      0           0
#> Libyan Arab Jamahiriya                            0      0          13
#> Lithuania                                         0      0           0
#> Luxembourg                                        0      1           0
#> Macedonia, The Former Yugoslav Republic Of        0      0           0
#> Madagascar                                        0      0           0
#> Malawi                                            0      0           0
#> Malaysia                                          0      1           0
#> Mali                                              0      0           0
#> Mauritania                                        0      0           0
#> Mauritius                                         0      0           0
#> Mexico                                            0      0           0
#> Moldova, Republic Of                              0      0           0
#> Mongolia                                          0      0           0
#> Morocco                                           0      0           0
#> Mozambique                                        0      0           0
#> Myanmar                                           0      0           2
#> Nepal                                             0      0           0
#> Netherlands                                       9      0           2
#> New Zealand                                       0      0           0
#> Nicaragua                                         0      0           0
#> Niger                                             0      0           0
#> Nigeria                                           0      0           1
#> Norway                                            0      0           0
#> Oman                                              0      0           0
#> Pakistan                                          0      1           2
#> Panama                                            0      0           0
#> Papua New Guinea                                  0      0           0
#> Paraguay                                          0      0           1
#> Peru                                              0      0           0
#> Philippines                                       0      0           1
#> Poland                                            0      8           0
#> Portugal                                          0      0           0
#> Qatar                                             0      0           0
#> Romania                                           0      0           0
#> Russian Federation                                2      1           4
#> Rwanda                                            0      0           0
#> Saudi Arabia                                      0      0           0
#> Senegal                                           0      1           1
#> Sierra Leone                                      0      0           0
#> Singapore                                         0      0           6
#> Slovakia                                          0      1           0
#> Slovenia                                          0      0           0
#> Solomon Islands                                   0      0           0
#> Somalia                                           0      4           4
#> South Africa                                      0      1           0
#> Spain                                             0      0           1
#> Sri Lanka                                         0      0           2
#> Sudan                                             0      0           1
#> Suriname                                         NA      0           0
#> Sweden                                            0     NA           2
#> Switzerland                                       0      2          NA
#> Syrian Arab Republic                              0      4           0
#> Tajikistan                                        2      0           0
#> Tanzania, United Republic Of                      0      0           0
#> Thailand                                          0      1           0
#> Togo                                              0      0           0
#> Trinidad And Tobago                               0      0           0
#> Tunisia                                           0      1           0
#> Turkmenistan                                      0      0           0
#> Uganda                                            0      0           0
#> Ukraine                                           0      2           0
#> United Arab Emirates                              0      0           0
#> United Kingdom                                    0      2           0
#> United States                                     2      8           5
#> Uruguay                                           0      0           0
#> Uzbekistan                                        0      0           0
#> Vietnam                                           0      0           0
#> Yemen                                             0      7           1
#> Zambia                                            0      0           0
#> Zimbabwe                                          0      0           4
#>                                            Syrian Arab Republic Tajikistan
#> Afghanistan                                                   0         18
#> Albania                                                       0          0
#> Algeria                                                       0          0
#> Angola                                                        0          0
#> Argentina                                                     0          0
#> Armenia                                                       0          0
#> Australia                                                     0          0
#> Austria                                                       0          0
#> Azerbaijan                                                    0          0
#> Bahrain                                                       2          0
#> Bangladesh                                                    0          4
#> Belarus                                                       0          0
#> Belgium                                                       0          0
#> Benin                                                         0          0
#> Bolivia, Plurinational State Of                               0          0
#> Bosnia And Herzegovina                                        0          0
#> Brazil                                                        0          0
#> Bulgaria                                                      0          0
#> Burkina Faso                                                  0          0
#> Burundi                                                       0          0
#> Cambodia                                                      0          0
#> Cameroon                                                      0          0
#> Canada                                                        0          0
#> Cape Verde                                                    0          0
#> Central African Republic                                      0          0
#> Chad                                                          0          0
#> Chile                                                         0          0
#> China                                                         0          0
#> Colombia                                                      0          0
#> Comoros                                                       0          0
#> Congo, Republic Of                                            0          0
#> Congo, The Democratic Republic Of                             0          0
#> Costa Rica                                                    0          0
#> Cote D'ivoire                                                 0          0
#> Croatia                                                       1          2
#> Cuba                                                          2          0
#> Cyprus                                                        0          0
#> Denmark                                                       0          0
#> Djibouti                                                      0          0
#> Dominican Republic                                            0          0
#> Ecuador                                                       0          0
#> Egypt                                                         4          0
#> El Salvador                                                   0          0
#> Equatorial Guinea                                             0          0
#> Estonia                                                       0          0
#> Ethiopia                                                      0          0
#> Fiji                                                          0          0
#> Finland                                                       0          0
#> France                                                        1          0
#> Gabon                                                         0          0
#> Gambia                                                        0          0
#> Georgia                                                       0          0
#> Germany                                                       3          0
#> Ghana                                                         0          0
#> Greece                                                        1          0
#> Guatemala                                                     0          0
#> Guinea                                                        0          0
#> Guinea-Bissau                                                 0          0
#> Guyana                                                        0          0
#> Haiti                                                         0          0
#> Honduras                                                      0          0
#> Hungary                                                       0          0
#> India                                                         0          1
#> Indonesia                                                     2          0
#> Iran, Islamic Republic Of                                     3          1
#> Iraq                                                          4          0
#> Ireland                                                       0          0
#> Israel                                                       10          0
#> Italy                                                         0          0
#> Jamaica                                                       0          0
#> Japan                                                         3          6
#> Jordan                                                        0          0
#> Kazakhstan                                                    0          2
#> Kenya                                                         0          0
#> Korea, Democratic People's Republic Of                        0          0
#> Korea, Republic Of                                            0          0
#> Kuwait                                                        0          0
#> Kyrgyzstan                                                    0          3
#> Lao People's Democratic Republic                              0          0
#> Latvia                                                        0          0
#> Lebanon                                                      28          0
#> Liberia                                                       0          0
#> Libyan Arab Jamahiriya                                        0          0
#> Lithuania                                                     0          0
#> Luxembourg                                                    0          0
#> Macedonia, The Former Yugoslav Republic Of                    0          0
#> Madagascar                                                    0          0
#> Malawi                                                        0          0
#> Malaysia                                                      0          0
#> Mali                                                          0          0
#> Mauritania                                                    0          0
#> Mauritius                                                     0          0
#> Mexico                                                        0          0
#> Moldova, Republic Of                                          0          0
#> Mongolia                                                      0          0
#> Morocco                                                       0          0
#> Mozambique                                                    0          0
#> Myanmar                                                       0          0
#> Nepal                                                         0          0
#> Netherlands                                                   0          0
#> New Zealand                                                   0          0
#> Nicaragua                                                     0          0
#> Niger                                                         0          0
#> Nigeria                                                      13          0
#> Norway                                                        0          0
#> Oman                                                          0          0
#> Pakistan                                                      1          2
#> Panama                                                        0          0
#> Papua New Guinea                                              0          0
#> Paraguay                                                      0          0
#> Peru                                                          0          0
#> Philippines                                                   0          0
#> Poland                                                        0          0
#> Portugal                                                      0          0
#> Qatar                                                         1          0
#> Romania                                                       0          0
#> Russian Federation                                            1          7
#> Rwanda                                                        0          0
#> Saudi Arabia                                                  2          0
#> Senegal                                                       0          0
#> Sierra Leone                                                  0          0
#> Singapore                                                     0          0
#> Slovakia                                                      0          0
#> Slovenia                                                      0          0
#> Solomon Islands                                               0          0
#> Somalia                                                       0          0
#> South Africa                                                  0          0
#> Spain                                                         3          0
#> Sri Lanka                                                     0          0
#> Sudan                                                         0          0
#> Suriname                                                      0          2
#> Sweden                                                        4          0
#> Switzerland                                                   0          0
#> Syrian Arab Republic                                         NA          0
#> Tajikistan                                                    0         NA
#> Tanzania, United Republic Of                                  0          0
#> Thailand                                                      0          0
#> Togo                                                          0          0
#> Trinidad And Tobago                                           0          0
#> Tunisia                                                       0          0
#> Turkmenistan                                                  0          0
#> Uganda                                                        0          0
#> Ukraine                                                       0          0
#> United Arab Emirates                                          1          0
#> United Kingdom                                                1          0
#> United States                                                39          2
#> Uruguay                                                       0          0
#> Uzbekistan                                                    0         28
#> Vietnam                                                       0          0
#> Yemen                                                         4          0
#> Zambia                                                        0          0
#> Zimbabwe                                                      0          0
#>                                            Tanzania, United Republic Of
#> Afghanistan                                                           0
#> Albania                                                               0
#> Algeria                                                               0
#> Angola                                                                0
#> Argentina                                                             0
#> Armenia                                                               0
#> Australia                                                             0
#> Austria                                                               0
#> Azerbaijan                                                            0
#> Bahrain                                                               0
#> Bangladesh                                                            0
#> Belarus                                                               0
#> Belgium                                                               0
#> Benin                                                                 0
#> Bolivia, Plurinational State Of                                       0
#> Bosnia And Herzegovina                                                0
#> Brazil                                                                0
#> Bulgaria                                                              2
#> Burkina Faso                                                          0
#> Burundi                                                               0
#> Cambodia                                                              0
#> Cameroon                                                              0
#> Canada                                                                0
#> Cape Verde                                                            0
#> Central African Republic                                              0
#> Chad                                                                  0
#> Chile                                                                 0
#> China                                                                 0
#> Colombia                                                              0
#> Comoros                                                               2
#> Congo, Republic Of                                                    0
#> Congo, The Democratic Republic Of                                     0
#> Costa Rica                                                            0
#> Cote D'ivoire                                                         0
#> Croatia                                                               0
#> Cuba                                                                  0
#> Cyprus                                                                0
#> Denmark                                                               0
#> Djibouti                                                              0
#> Dominican Republic                                                    0
#> Ecuador                                                               0
#> Egypt                                                                 0
#> El Salvador                                                           0
#> Equatorial Guinea                                                     0
#> Estonia                                                               0
#> Ethiopia                                                              2
#> Fiji                                                                  0
#> Finland                                                               0
#> France                                                                0
#> Gabon                                                                 0
#> Gambia                                                                0
#> Georgia                                                               0
#> Germany                                                               0
#> Ghana                                                                 0
#> Greece                                                                0
#> Guatemala                                                             0
#> Guinea                                                                2
#> Guinea-Bissau                                                         0
#> Guyana                                                                0
#> Haiti                                                                 0
#> Honduras                                                              0
#> Hungary                                                               0
#> India                                                                 0
#> Indonesia                                                             0
#> Iran, Islamic Republic Of                                             0
#> Iraq                                                                  0
#> Ireland                                                               0
#> Israel                                                                0
#> Italy                                                                 0
#> Jamaica                                                               0
#> Japan                                                                 1
#> Jordan                                                                0
#> Kazakhstan                                                            0
#> Kenya                                                                 7
#> Korea, Democratic People's Republic Of                                0
#> Korea, Republic Of                                                    0
#> Kuwait                                                                0
#> Kyrgyzstan                                                            0
#> Lao People's Democratic Republic                                      0
#> Latvia                                                                0
#> Lebanon                                                               0
#> Liberia                                                               0
#> Libyan Arab Jamahiriya                                                1
#> Lithuania                                                             0
#> Luxembourg                                                            0
#> Macedonia, The Former Yugoslav Republic Of                            0
#> Madagascar                                                            0
#> Malawi                                                                0
#> Malaysia                                                              0
#> Mali                                                                  0
#> Mauritania                                                            0
#> Mauritius                                                             0
#> Mexico                                                                0
#> Moldova, Republic Of                                                  0
#> Mongolia                                                              0
#> Morocco                                                               0
#> Mozambique                                                            0
#> Myanmar                                                               0
#> Nepal                                                                 0
#> Netherlands                                                           0
#> New Zealand                                                           0
#> Nicaragua                                                             0
#> Niger                                                                 0
#> Nigeria                                                               0
#> Norway                                                                0
#> Oman                                                                  0
#> Pakistan                                                              0
#> Panama                                                                0
#> Papua New Guinea                                                      0
#> Paraguay                                                              0
#> Peru                                                                  0
#> Philippines                                                           0
#> Poland                                                                0
#> Portugal                                                              0
#> Qatar                                                                 0
#> Romania                                                               0
#> Russian Federation                                                    0
#> Rwanda                                                                1
#> Saudi Arabia                                                          0
#> Senegal                                                               0
#> Sierra Leone                                                          0
#> Singapore                                                             1
#> Slovakia                                                              0
#> Slovenia                                                              0
#> Solomon Islands                                                       0
#> Somalia                                                               2
#> South Africa                                                          0
#> Spain                                                                 0
#> Sri Lanka                                                             0
#> Sudan                                                                 0
#> Suriname                                                              0
#> Sweden                                                                0
#> Switzerland                                                           0
#> Syrian Arab Republic                                                  0
#> Tajikistan                                                            0
#> Tanzania, United Republic Of                                         NA
#> Thailand                                                              1
#> Togo                                                                  0
#> Trinidad And Tobago                                                   0
#> Tunisia                                                               0
#> Turkmenistan                                                          0
#> Uganda                                                                3
#> Ukraine                                                               0
#> United Arab Emirates                                                  0
#> United Kingdom                                                        1
#> United States                                                         6
#> Uruguay                                                               0
#> Uzbekistan                                                            0
#> Vietnam                                                               0
#> Yemen                                                                 0
#> Zambia                                                                1
#> Zimbabwe                                                              0
#>                                            Thailand Togo Trinidad And Tobago
#> Afghanistan                                       0    0                   1
#> Albania                                           0    0                   0
#> Algeria                                           0    2                   0
#> Angola                                            0    2                   0
#> Argentina                                         0    0                   0
#> Armenia                                           0    0                   0
#> Australia                                        50    0                   0
#> Austria                                           0    0                   0
#> Azerbaijan                                        0    0                   0
#> Bahrain                                           0    0                   0
#> Bangladesh                                        0    0                   2
#> Belarus                                           0    0                   0
#> Belgium                                           1    0                   0
#> Benin                                             0    0                   0
#> Bolivia, Plurinational State Of                   0    0                   0
#> Bosnia And Herzegovina                            0    0                   0
#> Brazil                                            1    0                   0
#> Bulgaria                                          0    0                   0
#> Burkina Faso                                      0    0                   0
#> Burundi                                           0    0                   0
#> Cambodia                                        139    0                   0
#> Cameroon                                          1    0                   0
#> Canada                                            2    0                   0
#> Cape Verde                                        0    0                   0
#> Central African Republic                          0    1                   0
#> Chad                                              0    0                   0
#> Chile                                             0    0                   0
#> China                                            12    0                   0
#> Colombia                                          4    0                   0
#> Comoros                                           0    0                   0
#> Congo, Republic Of                                0    0                   0
#> Congo, The Democratic Republic Of                 0    0                   0
#> Costa Rica                                        0    0                   0
#> Cote D'ivoire                                     0    0                   0
#> Croatia                                           0    0                   0
#> Cuba                                              0    0                   0
#> Cyprus                                            0    0                   0
#> Denmark                                           0    0                   0
#> Djibouti                                          0    0                   0
#> Dominican Republic                                0    0                   0
#> Ecuador                                           0    0                   0
#> Egypt                                             0    0                   0
#> El Salvador                                       0    0                   0
#> Equatorial Guinea                                 0    0                   0
#> Estonia                                           0    0                   0
#> Ethiopia                                          1    0                   0
#> Fiji                                              0    0                   0
#> Finland                                           0    0                   0
#> France                                            4    0                   0
#> Gabon                                             0    0                   0
#> Gambia                                            0    0                   0
#> Georgia                                           0    0                   0
#> Germany                                           3    0                   0
#> Ghana                                             1    0                   0
#> Greece                                            0    0                   0
#> Guatemala                                         1    0                   0
#> Guinea                                            2    2                   0
#> Guinea-Bissau                                     0    0                   0
#> Guyana                                            0    0                   0
#> Haiti                                             0    2                   0
#> Honduras                                          0    0                   0
#> Hungary                                           0    0                   0
#> India                                             3    0                   0
#> Indonesia                                         2    0                   0
#> Iran, Islamic Republic Of                        46    0                   0
#> Iraq                                              1    0                   3
#> Ireland                                           0    0                   0
#> Israel                                            3    0                   0
#> Italy                                             0    0                   0
#> Jamaica                                           0    0                   0
#> Japan                                            21    0                   0
#> Jordan                                            0    0                   0
#> Kazakhstan                                       10    0                   0
#> Kenya                                             3    0                   2
#> Korea, Democratic People's Republic Of            2    0                   0
#> Korea, Republic Of                                1    0                   0
#> Kuwait                                            0    0                   0
#> Kyrgyzstan                                        0    0                   0
#> Lao People's Democratic Republic                  2    0                   0
#> Latvia                                            0    0                   0
#> Lebanon                                           1    0                   0
#> Liberia                                           0    0                   0
#> Libyan Arab Jamahiriya                            0    0                   0
#> Lithuania                                         0    0                   0
#> Luxembourg                                        0    0                   0
#> Macedonia, The Former Yugoslav Republic Of        0    0                   0
#> Madagascar                                        0    0                   0
#> Malawi                                            0    0                   0
#> Malaysia                                         16    0                   0
#> Mali                                              0    0                   0
#> Mauritania                                        0    0                   0
#> Mauritius                                         0    0                   0
#> Mexico                                            3    0                   0
#> Moldova, Republic Of                              0    0                   0
#> Mongolia                                          0    0                   0
#> Morocco                                           0    0                   0
#> Mozambique                                        5    0                   0
#> Myanmar                                          40    0                   0
#> Nepal                                             0    0                   0
#> Netherlands                                       0    0                   0
#> New Zealand                                       5    0                   0
#> Nicaragua                                         0    0                   0
#> Niger                                             0    1                   0
#> Nigeria                                           5    0                   4
#> Norway                                            0    0                   0
#> Oman                                              0    0                   0
#> Pakistan                                         32    0                   4
#> Panama                                            0    0                   0
#> Papua New Guinea                                  0    0                   0
#> Paraguay                                          0    0                   0
#> Peru                                              0    0                   0
#> Philippines                                      15    0                   0
#> Poland                                            1    0                   0
#> Portugal                                          0    0                   0
#> Qatar                                             0    0                   0
#> Romania                                           0    0                   0
#> Russian Federation                              107    0                   4
#> Rwanda                                            0    0                   0
#> Saudi Arabia                                      4    0                   0
#> Senegal                                           0    0                   0
#> Sierra Leone                                      0    0                   0
#> Singapore                                         3    0                   0
#> Slovakia                                          2    0                   0
#> Slovenia                                          0    0                   0
#> Solomon Islands                                   0    0                   0
#> Somalia                                           5    0                   0
#> South Africa                                      3    0                   1
#> Spain                                             1    0                   0
#> Sri Lanka                                        14    0                   2
#> Sudan                                             0    0                   0
#> Suriname                                          0    0                   0
#> Sweden                                            1    0                   0
#> Switzerland                                       0    0                   0
#> Syrian Arab Republic                              0    0                   0
#> Tajikistan                                        0    0                   0
#> Tanzania, United Republic Of                      1    0                   0
#> Thailand                                         NA    0                   0
#> Togo                                              0   NA                   0
#> Trinidad And Tobago                               0    0                  NA
#> Tunisia                                           0    0                   0
#> Turkmenistan                                      0    0                   0
#> Uganda                                            1    0                   1
#> Ukraine                                           0    0                   0
#> United Arab Emirates                             13    0                   0
#> United Kingdom                                    6    1                   1
#> United States                                    49    1                   1
#> Uruguay                                           0    0                   0
#> Uzbekistan                                        0    0                   0
#> Vietnam                                          19    0                   0
#> Yemen                                             1    0                   0
#> Zambia                                            0    0                   0
#> Zimbabwe                                          0    0                   3
#>                                            Tunisia Turkmenistan Uganda Ukraine
#> Afghanistan                                      0            0      0       0
#> Albania                                          0            0      0       0
#> Algeria                                          0            0      0       0
#> Angola                                           0            0      0       0
#> Argentina                                        0            0      1       0
#> Armenia                                          0            0      0       1
#> Australia                                        1            0      0       0
#> Austria                                          0            0      0       1
#> Azerbaijan                                       0            0      0       1
#> Bahrain                                          0            0      0       0
#> Bangladesh                                       0            0      0       0
#> Belarus                                          0            0      0       6
#> Belgium                                          0            0      0       0
#> Benin                                            0            0      0       0
#> Bolivia, Plurinational State Of                  0            0      0       0
#> Bosnia And Herzegovina                           0            0      0       0
#> Brazil                                           0            0      0       0
#> Bulgaria                                         0            0      0       0
#> Burkina Faso                                     0            0      0       0
#> Burundi                                          0            0      2       0
#> Cambodia                                         0            0      0       0
#> Cameroon                                         0            0      0       0
#> Canada                                           0            0      0       1
#> Cape Verde                                       0            0      0       0
#> Central African Republic                         0            0     30       0
#> Chad                                             0            0      1       0
#> Chile                                            0            0      0       0
#> China                                            0            0      4       1
#> Colombia                                         0            0      0       0
#> Comoros                                          0            0      0       0
#> Congo, Republic Of                               0            0     18       0
#> Congo, The Democratic Republic Of                0            0     13       1
#> Costa Rica                                       0            0      0       0
#> Cote D'ivoire                                    0            0      0       1
#> Croatia                                          0            0      0       0
#> Cuba                                             1            0      0       0
#> Cyprus                                           0            0      0       0
#> Denmark                                          0            0      1       0
#> Djibouti                                         0            0      0       0
#> Dominican Republic                               0            0      0       0
#> Ecuador                                          0            0      0       0
#> Egypt                                            4            0      0       0
#> El Salvador                                      0            0      0       0
#> Equatorial Guinea                                0            0      0       0
#> Estonia                                          0            0      0       0
#> Ethiopia                                         0            0      4       0
#> Fiji                                             0            0      0       0
#> Finland                                          0            0      1       2
#> France                                           2            0      0       4
#> Gabon                                            0            0      0       0
#> Gambia                                           0            0      0       0
#> Georgia                                          0            0      0      14
#> Germany                                          0            0      0       4
#> Ghana                                            0            0      0       0
#> Greece                                           0            0      0       1
#> Guatemala                                        0            0      0       0
#> Guinea                                           0            0      1       0
#> Guinea-Bissau                                    0            0      0       0
#> Guyana                                           0            0      0       0
#> Haiti                                            0            0      0       0
#> Honduras                                         0            0      0       0
#> Hungary                                          0            0      0       0
#> India                                            0            0      1       0
#> Indonesia                                        0            0      0       0
#> Iran, Islamic Republic Of                        0            1      0       0
#> Iraq                                             2            0      0       0
#> Ireland                                          0            0      0       0
#> Israel                                           3            0      0       6
#> Italy                                            0            0      0       0
#> Jamaica                                          0            0      0       0
#> Japan                                            0            0      0       0
#> Jordan                                           0            0      0       0
#> Kazakhstan                                       0            2      0       3
#> Kenya                                            0            0     27       0
#> Korea, Democratic People's Republic Of           0            0      0       1
#> Korea, Republic Of                               0            0      6       0
#> Kuwait                                           0            0      0       0
#> Kyrgyzstan                                       0            0      0       0
#> Lao People's Democratic Republic                 0            0      0       0
#> Latvia                                           0            0      0       0
#> Lebanon                                          0            0      1       0
#> Liberia                                          0            0      0       0
#> Libyan Arab Jamahiriya                           1            0      0       0
#> Lithuania                                        0            0      0       0
#> Luxembourg                                       0            0      0       0
#> Macedonia, The Former Yugoslav Republic Of       0            0      0       0
#> Madagascar                                       0            0      0       0
#> Malawi                                           0            0      0       0
#> Malaysia                                         0            0      0       0
#> Mali                                             0            0      0       0
#> Mauritania                                       1            0      1       0
#> Mauritius                                        0            0      0       0
#> Mexico                                           0            0      0       0
#> Moldova, Republic Of                             0            0      0       2
#> Mongolia                                         0            0      0       1
#> Morocco                                          0            0      0       0
#> Mozambique                                       0            0      0       0
#> Myanmar                                          0            0      0       0
#> Nepal                                            0            0      0       0
#> Netherlands                                      0            0      2       0
#> New Zealand                                      0            0      0       0
#> Nicaragua                                        0            0      0       0
#> Niger                                            0            0      0       0
#> Nigeria                                          0            0      0       1
#> Norway                                           0            0      0       0
#> Oman                                             0            0      0       0
#> Pakistan                                         0            0      2       0
#> Panama                                           0            0      0       0
#> Papua New Guinea                                 0            0      0       0
#> Paraguay                                         0            0      0       0
#> Peru                                             0            0      0       0
#> Philippines                                      0            0      0       0
#> Poland                                           0            0      0       3
#> Portugal                                         0            0      0       1
#> Qatar                                            0            0      0       0
#> Romania                                          0            0      0       1
#> Russian Federation                               0            1      0      29
#> Rwanda                                           0            0     30       0
#> Saudi Arabia                                     0            0      0       0
#> Senegal                                          0            0      1       0
#> Sierra Leone                                     0            0      0       0
#> Singapore                                        0            0      0       0
#> Slovakia                                         0            0      0       2
#> Slovenia                                         0            0      0       0
#> Solomon Islands                                  0            0      0       0
#> Somalia                                          0            0     73       2
#> South Africa                                     2            0      5       1
#> Spain                                            0            0      0       0
#> Sri Lanka                                        0            0      0       0
#> Sudan                                            0            0     25       0
#> Suriname                                         0            0      0       0
#> Sweden                                           1            0      0       2
#> Switzerland                                      0            0      0       0
#> Syrian Arab Republic                             0            0      0       0
#> Tajikistan                                       0            0      0       0
#> Tanzania, United Republic Of                     0            0      3       0
#> Thailand                                         0            0      1       0
#> Togo                                             0            0      0       0
#> Trinidad And Tobago                              0            0      1       0
#> Tunisia                                         NA            0      0       0
#> Turkmenistan                                     0           NA      0       0
#> Uganda                                           0            0     NA       1
#> Ukraine                                          0            0      1      NA
#> United Arab Emirates                             0            0      1       0
#> United Kingdom                                   0            0      2       0
#> United States                                    0            2      3       6
#> Uruguay                                          0            0      0       0
#> Uzbekistan                                       0            3      0       0
#> Vietnam                                          0            0      0       0
#> Yemen                                            0            0      0       1
#> Zambia                                           0            0      0       0
#> Zimbabwe                                         0            0      9       0
#>                                            United Arab Emirates United Kingdom
#> Afghanistan                                                   4            116
#> Albania                                                       0              2
#> Algeria                                                       0              7
#> Angola                                                        0              1
#> Argentina                                                     0              4
#> Armenia                                                       0              0
#> Australia                                                     3            103
#> Austria                                                       0              3
#> Azerbaijan                                                    2              0
#> Bahrain                                                       0              2
#> Bangladesh                                                    2              1
#> Belarus                                                       0              0
#> Belgium                                                       0              0
#> Benin                                                         0              0
#> Bolivia, Plurinational State Of                               0              0
#> Bosnia And Herzegovina                                        0            109
#> Brazil                                                        0              5
#> Bulgaria                                                      0              1
#> Burkina Faso                                                  0              0
#> Burundi                                                       0              0
#> Cambodia                                                      0              2
#> Cameroon                                                      0              0
#> Canada                                                        3              6
#> Cape Verde                                                    0              0
#> Central African Republic                                      0              0
#> Chad                                                          0              0
#> Chile                                                         0              6
#> China                                                         2              4
#> Colombia                                                      2              3
#> Comoros                                                       0              0
#> Congo, Republic Of                                            0              0
#> Congo, The Democratic Republic Of                             0              0
#> Costa Rica                                                    0              0
#> Cote D'ivoire                                                 2              0
#> Croatia                                                       0             13
#> Cuba                                                          0              0
#> Cyprus                                                        0              1
#> Denmark                                                       0              1
#> Djibouti                                                      0              0
#> Dominican Republic                                            0              0
#> Ecuador                                                       0              0
#> Egypt                                                         1              2
#> El Salvador                                                   0              0
#> Equatorial Guinea                                             0              0
#> Estonia                                                       0              0
#> Ethiopia                                                      0              2
#> Fiji                                                          0              0
#> Finland                                                       0              0
#> France                                                        3              6
#> Gabon                                                         0              0
#> Gambia                                                        0              0
#> Georgia                                                       0              1
#> Germany                                                       1              6
#> Ghana                                                         0              1
#> Greece                                                        0              1
#> Guatemala                                                     0              0
#> Guinea                                                        0              0
#> Guinea-Bissau                                                 0              0
#> Guyana                                                        0              0
#> Haiti                                                         0              0
#> Honduras                                                      0              0
#> Hungary                                                       0              0
#> India                                                        25             17
#> Indonesia                                                     1              2
#> Iran, Islamic Republic Of                                     7             29
#> Iraq                                                          7             41
#> Ireland                                                       0             20
#> Israel                                                       14            122
#> Italy                                                         0              2
#> Jamaica                                                       0              4
#> Japan                                                         0             11
#> Jordan                                                        4              2
#> Kazakhstan                                                    0              0
#> Kenya                                                         2              4
#> Korea, Democratic People's Republic Of                        0              1
#> Korea, Republic Of                                            0              0
#> Kuwait                                                        1              1
#> Kyrgyzstan                                                    0              4
#> Lao People's Democratic Republic                              0              0
#> Latvia                                                        0              3
#> Lebanon                                                       5              5
#> Liberia                                                       0              1
#> Libyan Arab Jamahiriya                                        0              1
#> Lithuania                                                     0              1
#> Luxembourg                                                    0              0
#> Macedonia, The Former Yugoslav Republic Of                    0              0
#> Madagascar                                                    0              0
#> Malawi                                                        0              1
#> Malaysia                                                      0              1
#> Mali                                                          0              0
#> Mauritania                                                    0              0
#> Mauritius                                                     0              0
#> Mexico                                                        1              0
#> Moldova, Republic Of                                          0              0
#> Mongolia                                                      0              0
#> Morocco                                                       0              0
#> Mozambique                                                    0              0
#> Myanmar                                                       0              0
#> Nepal                                                         0              1
#> Netherlands                                                   0              1
#> New Zealand                                                   0              1
#> Nicaragua                                                     0              0
#> Niger                                                         0              0
#> Nigeria                                                       9              7
#> Norway                                                        0              0
#> Oman                                                          0              0
#> Pakistan                                                      3             21
#> Panama                                                        0              1
#> Papua New Guinea                                              0              2
#> Paraguay                                                      0              0
#> Peru                                                          0              2
#> Philippines                                                   0              1
#> Poland                                                        0              7
#> Portugal                                                      0              9
#> Qatar                                                         0              0
#> Romania                                                       0              2
#> Russian Federation                                            6             50
#> Rwanda                                                        0              0
#> Saudi Arabia                                                  2             19
#> Senegal                                                       0              0
#> Sierra Leone                                                  0              0
#> Singapore                                                     0             31
#> Slovakia                                                      0              0
#> Slovenia                                                      0              0
#> Solomon Islands                                               0              0
#> Somalia                                                       0             10
#> South Africa                                                  1             12
#> Spain                                                         0             20
#> Sri Lanka                                                     0              9
#> Sudan                                                         0              3
#> Suriname                                                      0              0
#> Sweden                                                        0              2
#> Switzerland                                                   0              0
#> Syrian Arab Republic                                          1              1
#> Tajikistan                                                    0              0
#> Tanzania, United Republic Of                                  0              1
#> Thailand                                                     13              6
#> Togo                                                          0              1
#> Trinidad And Tobago                                           0              1
#> Tunisia                                                       0              0
#> Turkmenistan                                                  0              0
#> Uganda                                                        1              2
#> Ukraine                                                       0              0
#> United Arab Emirates                                         NA             17
#> United Kingdom                                               17             NA
#> United States                                                 6            100
#> Uruguay                                                       0              0
#> Uzbekistan                                                    0              3
#> Vietnam                                                       0              2
#> Yemen                                                         2             13
#> Zambia                                                        0              3
#> Zimbabwe                                                      0              6
#>                                            United States Uruguay Uzbekistan
#> Afghanistan                                         1018       0         10
#> Albania                                                1       0          0
#> Algeria                                                4       0          0
#> Angola                                                 0       0          0
#> Argentina                                              7       2          0
#> Armenia                                                6       0          0
#> Australia                                            142       0          0
#> Austria                                                2       0          0
#> Azerbaijan                                             3       0          0
#> Bahrain                                                0       0          0
#> Bangladesh                                             3       0          0
#> Belarus                                               13       0          0
#> Belgium                                               10       0          0
#> Benin                                                  3       0          0
#> Bolivia, Plurinational State Of                        5       1          0
#> Bosnia And Herzegovina                                 7       0          0
#> Brazil                                                 3       1          0
#> Bulgaria                                               4       0          0
#> Burkina Faso                                           1       0          0
#> Burundi                                                0       0          0
#> Cambodia                                              11       0          0
#> Cameroon                                               0       0          0
#> Canada                                                63       0          0
#> Cape Verde                                             3       0          0
#> Central African Republic                               1       0          0
#> Chad                                                   0       0          0
#> Chile                                                 10       3          0
#> China                                                 94       0          2
#> Colombia                                              48       1          0
#> Comoros                                                0       0          0
#> Congo, Republic Of                                     1       0          0
#> Congo, The Democratic Republic Of                      2       0          0
#> Costa Rica                                             1       0          0
#> Cote D'ivoire                                         10       0          0
#> Croatia                                                2       0          0
#> Cuba                                                  97       1          0
#> Cyprus                                                 2       0          0
#> Denmark                                                3       0          0
#> Djibouti                                               0       0          0
#> Dominican Republic                                     5       0          0
#> Ecuador                                               14       0          0
#> Egypt                                                 17       0          1
#> El Salvador                                           11       0          0
#> Equatorial Guinea                                      0       0          0
#> Estonia                                                2       0          0
#> Ethiopia                                               2       0          0
#> Fiji                                                   1       0          0
#> Finland                                                2       0          0
#> France                                                10       1          1
#> Gabon                                                  0       0          0
#> Gambia                                                 2       0          0
#> Georgia                                               23       0          0
#> Germany                                               71       0          0
#> Ghana                                                  0       0          0
#> Greece                                                 6       1          0
#> Guatemala                                             33       0          0
#> Guinea                                                 0       0          0
#> Guinea-Bissau                                          1       0          0
#> Guyana                                                 1       0          0
#> Haiti                                                 63       0          0
#> Honduras                                              27       0          0
#> Hungary                                                2       0          0
#> India                                                 65       0          0
#> Indonesia                                            100       0          1
#> Iran, Islamic Republic Of                            683       0          0
#> Iraq                                                 260       0          0
#> Ireland                                                4       0          0
#> Israel                                                54       1          4
#> Italy                                                 13       0          0
#> Jamaica                                               16       0          0
#> Japan                                                108       0          0
#> Jordan                                                21       0          0
#> Kazakhstan                                             1       0          3
#> Kenya                                                 10       0          0
#> Korea, Democratic People's Republic Of               165       0          0
#> Korea, Republic Of                                    71       1         10
#> Kuwait                                                 5       0          0
#> Kyrgyzstan                                             6       0        273
#> Lao People's Democratic Republic                       1       0          0
#> Latvia                                                 0       0          0
#> Lebanon                                               42       0          0
#> Liberia                                               11       0          0
#> Libyan Arab Jamahiriya                                 7       0          0
#> Lithuania                                              1       0          0
#> Luxembourg                                             0       0          0
#> Macedonia, The Former Yugoslav Republic Of             0       0          0
#> Madagascar                                             0       0          0
#> Malawi                                                 0       0          0
#> Malaysia                                               4       0          0
#> Mali                                                   2       0          0
#> Mauritania                                             0       0          0
#> Mauritius                                              1       0          0
#> Mexico                                               211       2          0
#> Moldova, Republic Of                                   0       0          0
#> Mongolia                                               1       0          0
#> Morocco                                                0       0          0
#> Mozambique                                             2       0          0
#> Myanmar                                               43       0          0
#> Nepal                                                  0       0          0
#> Netherlands                                            8       2          0
#> New Zealand                                           20       0          0
#> Nicaragua                                              5       0          0
#> Niger                                                  0       0          0
#> Nigeria                                               48       0          0
#> Norway                                                 2       0          3
#> Oman                                                   0       0          0
#> Pakistan                                             869       0          4
#> Panama                                                65       0          0
#> Papua New Guinea                                       7       0          0
#> Paraguay                                               2       0          0
#> Peru                                                  14       0          0
#> Philippines                                           29       0          0
#> Poland                                                36       0          0
#> Portugal                                               1       0          0
#> Qatar                                                  1       0          0
#> Romania                                                6       0          0
#> Russian Federation                                   388       1          7
#> Rwanda                                                 6       0          0
#> Saudi Arabia                                           9       0          0
#> Senegal                                                1       0          0
#> Sierra Leone                                           1       0          0
#> Singapore                                              4       0          0
#> Slovakia                                               0       0          0
#> Slovenia                                               0       0          0
#> Solomon Islands                                        0       0          0
#> Somalia                                               60       0          0
#> South Africa                                           8       0          0
#> Spain                                                 29       1          1
#> Sri Lanka                                              1       0          1
#> Sudan                                                 14       0          0
#> Suriname                                               2       0          0
#> Sweden                                                 8       0          0
#> Switzerland                                            5       0          0
#> Syrian Arab Republic                                  39       0          0
#> Tajikistan                                             2       0         28
#> Tanzania, United Republic Of                           6       0          0
#> Thailand                                              49       0          0
#> Togo                                                   1       0          0
#> Trinidad And Tobago                                    1       0          0
#> Tunisia                                                0       0          0
#> Turkmenistan                                           2       0          3
#> Uganda                                                 3       0          0
#> Ukraine                                                6       0          0
#> United Arab Emirates                                   6       0          0
#> United Kingdom                                       100       0          3
#> United States                                         NA       4         13
#> Uruguay                                                4      NA          0
#> Uzbekistan                                            13       0         NA
#> Vietnam                                                6       0          0
#> Yemen                                                 68       0          0
#> Zambia                                                 2       0          0
#> Zimbabwe                                               6       0          0
#>                                            Vietnam Yemen Zambia Zimbabwe
#> Afghanistan                                      0     1      0        0
#> Albania                                          0     0      0        0
#> Algeria                                          0     0      0        0
#> Angola                                           0     0      0        0
#> Argentina                                        0     0      0        0
#> Armenia                                          0     0      0        0
#> Australia                                        9    25      0        7
#> Austria                                          0     0      0        0
#> Azerbaijan                                       0     0      0        0
#> Bahrain                                          0     0      0        0
#> Bangladesh                                       0     0      0        0
#> Belarus                                          0     0      0        0
#> Belgium                                          0     0      0        1
#> Benin                                            0     0      0        0
#> Bolivia, Plurinational State Of                  0     0      0        0
#> Bosnia And Herzegovina                           0     0      0        0
#> Brazil                                           0     0      0        0
#> Bulgaria                                         0     0      0        0
#> Burkina Faso                                     0     0      0        0
#> Burundi                                          0     0      0        1
#> Cambodia                                         4     0      0        0
#> Cameroon                                         0     0      0        0
#> Canada                                           1     0      0        0
#> Cape Verde                                       0     0      0        0
#> Central African Republic                         0     0      0        0
#> Chad                                             0     0      0        0
#> Chile                                            0     0      0        0
#> China                                            4    13      8        2
#> Colombia                                         0     0      0        0
#> Comoros                                          0     0      0        0
#> Congo, Republic Of                               0     0      0        1
#> Congo, The Democratic Republic Of                0     0      0        0
#> Costa Rica                                       0     0      0        0
#> Cote D'ivoire                                    0     0      0        1
#> Croatia                                          0     0      0        0
#> Cuba                                             0     1      0        1
#> Cyprus                                           0     0      0        0
#> Denmark                                          0     0      0        0
#> Djibouti                                         0     3      0        0
#> Dominican Republic                               0     0      0        0
#> Ecuador                                          0     0      0        4
#> Egypt                                            0     7      0        0
#> El Salvador                                      0     0      0        0
#> Equatorial Guinea                                0     0      0        0
#> Estonia                                          0     0      0        0
#> Ethiopia                                         0    75      0        1
#> Fiji                                             0     0      0        0
#> Finland                                          0     0      0        0
#> France                                           2    14      0        0
#> Gabon                                            0     0      0        0
#> Gambia                                           0     0      0        0
#> Georgia                                          0     0      0        0
#> Germany                                          5     6      0        2
#> Ghana                                            0     0      0        0
#> Greece                                           0     0      0        0
#> Guatemala                                        0     0      0        0
#> Guinea                                           0     0      0        0
#> Guinea-Bissau                                    0     0      0        0
#> Guyana                                           0     0      0        0
#> Haiti                                            0     0      0        0
#> Honduras                                         0     0      0        0
#> Hungary                                          5     0      0        0
#> India                                            0     4      1        1
#> Indonesia                                        6     0      0        0
#> Iran, Islamic Republic Of                        0     8      0        2
#> Iraq                                             1     1      0        0
#> Ireland                                          0     0      0        0
#> Israel                                           1     1      0        1
#> Italy                                            0     0      0        0
#> Jamaica                                          0     0      0        0
#> Japan                                            1     2      0        0
#> Jordan                                           0     3      0        0
#> Kazakhstan                                       0     0      0        0
#> Kenya                                            0     0      0        2
#> Korea, Democratic People's Republic Of           0     0      0        0
#> Korea, Republic Of                               1     0      0        0
#> Kuwait                                           0     0      0        0
#> Kyrgyzstan                                       0     0      0        0
#> Lao People's Democratic Republic                 1     0      0        0
#> Latvia                                           0     0      0        0
#> Lebanon                                          0     5      0        0
#> Liberia                                          0     0      0        0
#> Libyan Arab Jamahiriya                           0     0      0        2
#> Lithuania                                        0     0      0        0
#> Luxembourg                                       0     0      0        0
#> Macedonia, The Former Yugoslav Republic Of       0     0      0        0
#> Madagascar                                       0     0      0        0
#> Malawi                                           0     0      1        0
#> Malaysia                                         0     5      0        0
#> Mali                                             0     0      0        0
#> Mauritania                                       0     0      0        0
#> Mauritius                                        0     0      0        0
#> Mexico                                           0     0      0        1
#> Moldova, Republic Of                             0     0      0        0
#> Mongolia                                         0     0      0        0
#> Morocco                                          0     0      0        0
#> Mozambique                                       0     0      0        0
#> Myanmar                                          0     0      0        0
#> Nepal                                            0     2      0        0
#> Netherlands                                      0    29      0        0
#> New Zealand                                      1     0      0        0
#> Nicaragua                                        0     0      0        0
#> Niger                                            0     2      0        0
#> Nigeria                                          5     0      1        1
#> Norway                                           0     0      0        0
#> Oman                                             0     3      0        0
#> Pakistan                                         0    11      3       17
#> Panama                                           0     0      0        0
#> Papua New Guinea                                 0     0      0        0
#> Paraguay                                         0     0      0        0
#> Peru                                             0     0      0        0
#> Philippines                                      1     3      0        0
#> Poland                                           1     0      0        0
#> Portugal                                         0     0      0        0
#> Qatar                                            0     0      0        0
#> Romania                                          0     0      0        0
#> Russian Federation                               4     0      0        0
#> Rwanda                                           0     0      0        0
#> Saudi Arabia                                     0    30      0        0
#> Senegal                                          0     0      0        0
#> Sierra Leone                                     0     0      0        0
#> Singapore                                        2     0      0        0
#> Slovakia                                         2     0      0        0
#> Slovenia                                         0     0      0        0
#> Solomon Islands                                  0     0      0        0
#> Somalia                                          0    65      8        1
#> South Africa                                     1     0      2       13
#> Spain                                            0     1      0        0
#> Sri Lanka                                        0     0      0        0
#> Sudan                                            0     4      1        0
#> Suriname                                         0     0      0        0
#> Sweden                                           0     7      0        0
#> Switzerland                                      0     1      0        4
#> Syrian Arab Republic                             0     4      0        0
#> Tajikistan                                       0     0      0        0
#> Tanzania, United Republic Of                     0     0      1        0
#> Thailand                                        19     1      0        0
#> Togo                                             0     0      0        0
#> Trinidad And Tobago                              0     0      0        3
#> Tunisia                                          0     0      0        0
#> Turkmenistan                                     0     0      0        0
#> Uganda                                           0     0      0        9
#> Ukraine                                          0     1      0        0
#> United Arab Emirates                             0     2      0        0
#> United Kingdom                                   2    13      3        6
#> United States                                    6    68      2        6
#> Uruguay                                          0     0      0        0
#> Uzbekistan                                       0     0      0        0
#> Vietnam                                         NA     0      0        0
#> Yemen                                            0    NA      0        0
#> Zambia                                           0     0     NA        1
#> Zimbabwe                                         0     0      1       NA
#> 

# Example 4: Using numeric indices
# See specific positions in the network
peek(net,
    from = c(1, 3, 5, 7), # 1st, 3rd, 5th, 7th senders
    to = 1:10
) # first 10 receivers
#>             Afghanistan Albania Algeria Angola Argentina Armenia Australia
#> Afghanistan          NA       5       5      0         2      10       200
#> Algeria               5       1      NA      6         0       0         1
#> Argentina             2       0       0      8        NA      12         1
#> Australia           200       0       1      0         1       0        NA
#>             Austria Azerbaijan Bahrain
#> Afghanistan       2         13       2
#> Algeria           2          4       0
#> Argentina         2          7       2
#> Australia         2          1       4

# Example 5: Quick inspection patterns
# See who USA interacts with
peek(net, from = "United States", to = 10) # USA's ties to first 10 countries
#>               Afghanistan Albania Algeria Angola Argentina Armenia Australia
#> United States        3718      42      76     68       164     288       927
#>               Austria Azerbaijan Bahrain
#> United States      16        457      38
peek(net, from = 10, to = "United States") # First 10 countries' ties to USA
#>             United States
#> Afghanistan          3718
#> Albania                42
#> Algeria                76
#> Angola                 68
#> Argentina             164
#> Armenia               288
#> Australia             927
#> Austria                16
#> Azerbaijan            457
#> Bahrain                38
```
