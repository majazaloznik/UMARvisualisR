# The fetch function a chart's spec asks for

Most charts fetch straight from the database. A chart with
`aggregate = "Q"` in charts.csv gets its monthly data collapsed to
quarters first, via
[quarterly_fetch](https://majazaloznik.github.io/UMARvisualisR/reference/quarterly_fetch.md).
Chart-level by necessity: the wide table has one period column, so every
series on a chart shares a frequency.

## Usage

``` r
fetch_fn_for_chart(chart, base_fetch, min_obs = 3L)
```

## Arguments

- chart:

  one row of the charts table

- base_fetch:

  the underlying fetch function

- min_obs:

  passed to
  [aggregate_to_quarters](https://majazaloznik.github.io/UMARvisualisR/reference/aggregate_to_quarters.md)

## Value

a function with base_fetch's signature
