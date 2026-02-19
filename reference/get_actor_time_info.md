# Extract actor time range information from dyadic data

`get_actor_time_info` analyzes a longitudinal dyadic dataset to
determine when each actor enters and exits the network. Entry is defined
as the first time period in which an actor appears in any interaction
(as either sender or receiver), and exit as the last time period.

## Usage

``` r
get_actor_time_info(dyad_data, actor1, actor2, time)
```

## Arguments

- dyad_data:

  A data.frame containing longitudinal dyadic observations. Must include
  columns for two actors and time periods. Will be coerced to data.frame
  if a tibble or data.table is provided.

- actor1:

  Character string specifying the column name for the first actor in
  each dyad.

- actor2:

  Character string specifying the column name for the second actor in
  each dyad.

- time:

  Character string specifying the column name for time periods.

## Value

A data.frame with three columns containing actor-level time information:

- **actor**: Character vector of unique actor identifiers found in
  either actor1 or actor2 columns

- **min_time**: The earliest time period in which each actor appears in
  the data (entry point)

- **max_time**: The latest time period in which each actor appears in
  the data (exit point)

Actors are ordered as they appear in the aggregation, not alphabetically
or by time.

## Details

The function performs the following operations:

**Data processing:**

1.  Combines actor1 and actor2 columns into a single nodal format

2.  Aggregates by actor to find minimum and maximum time periods

3.  Returns a clean data.frame with one row per unique actor

**Use cases:**

Main usage in this package is to:

- Preparing actor existence information for the `actor_pds` parameter in
  [`netify()`](https://netify-dev.github.io/netify/reference/netify.md)

**Assumptions:**

- An actor is considered "present" in any time period where they appear
  in the data, regardless of their role (sender/receiver)

- Missing values in time periods are ignored when calculating min/max

- Actors must appear in at least one non-missing time period

## Note

The function assumes that presence in the data indicates network
participation. If actors can be temporarily absent from the network
while still being considered members, this function will not capture
such gaps.

## Author

Shahryar Minhas, Ha Eun Choi
