# Fail loud if the chart spec tables have structural problems

Fail loud if the chart spec tables have structural problems

## Usage

``` r
check_chart_specs(charts, chart_series)
```

## Arguments

- charts:

  tibble read from charts.csv

- chart_series:

  tibble read from chart_series.csv

## Value

Invisibly, the (empty) problems tibble, if it doesn't stop first.
