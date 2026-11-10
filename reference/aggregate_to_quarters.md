# Aggregate a monthly chart table to quarters

Collapses a monthly wide table to quarters. Not a formula helper: it
changes the row count, so it must run on the whole table at fetch time
rather than in a `formula` cell. Chart-level by nature, since one period
column means every series shares a frequency.

## Usage

``` r
aggregate_to_quarters(
  data,
  value_cols,
  period_col = "period_id",
  aggr_func = mean,
  min_obs = 3L
)
```

## Arguments

- data:

  wide table with a period column and one column per series

- value_cols:

  columns to aggregate

- period_col:

  name of the period column, monthly (YYYYMnn)

- aggr_func:

  aggregation function, e.g. mean or sum

- min_obs:

  months required before a quarter is reported; 3 (the default) allows
  only complete quarters

## Value

the table with one row per quarter, period column in YYYYQn form

## Details

A quarter with fewer than `min_obs` observations comes back NA rather
than as a partial aggregate — most important with `sum`, where two
months of a three-month quarter reads as a collapse rather than as
missing data. The resulting NA quarters are trimmed by
[chart_args_from_spec](https://majazaloznik.github.io/UMARvisualisR/reference/chart_args_from_spec.md)
if no plotted series has data there.
