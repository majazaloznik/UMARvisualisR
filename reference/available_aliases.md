# Aliases a formula at a given position may reference

`period_id`, every non-computed alias in the chart regardless of
position, and every computed alias at an earlier position. Mirrors the
evaluation order in
[build_chart_data](https://majazaloznik.github.io/UMARvisualisR/reference/build_chart_data.md):
fetched series are all available before any formula runs; computed
series are evaluated sequentially by position.

## Usage

``` r
available_aliases(chart_series, chart_id, position)
```

## Arguments

- chart_series:

  full chart_series table

- chart_id:

  chart to look in

- position:

  position of the formula row

## Value

character vector of aliases
