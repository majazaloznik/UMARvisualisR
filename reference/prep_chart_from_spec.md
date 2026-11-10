# Build a umar_chart from one chart's spec

[chart_args_from_spec](https://majazaloznik.github.io/UMARvisualisR/reference/chart_args_from_spec.md)
followed by
[prep_chart](https://majazaloznik.github.io/UMARvisualisR/reference/prep_chart.md).

## Usage

``` r
prep_chart_from_spec(chart, series_rows, wide, language = "si")
```

## Arguments

- chart:

  one row of the charts table

- series_rows:

  this chart's rows of the chart_series table (aliases filled)

- wide:

  the table from
  [build_chart_data](https://majazaloznik.github.io/UMARvisualisR/reference/build_chart_data.md)

- language:

  "si" or "en" — picks the `_sl`/`_en` columns

## Value

an object of class "umar_chart"
