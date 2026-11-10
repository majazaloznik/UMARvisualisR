# Validate the chart spec tables for structural/referential problems

Returns a tibble of problems (zero rows = clean). Pure and testable on
its own;
[check_chart_specs](https://majazaloznik.github.io/UMARvisualisR/reference/check_chart_specs.md)
wraps it with a fail-loud stop().

## Usage

``` r
validate_chart_specs(charts, chart_series)
```

## Arguments

- charts:

  tibble read from charts.csv

- chart_series:

  tibble read from chart_series.csv

## Value

A tibble with columns `check`, `chart_id`, `detail`, one row per
problem.

## Details

Titles are optional but must be given in both languages or neither.
Blank aliases on 'db' rows are filled via
[fill_default_aliases](https://majazaloznik.github.io/UMARvisualisR/reference/fill_default_aliases.md).
Transform columns follow the same rules
[prep_chart](https://majazaloznik.github.io/UMARvisualisR/reference/prep_chart.md)
enforces: `rolling` an integer \>= 2, `growth` one of YOY/QOQ/MOM,
`index` a year/quarter/month, growth and index mutually exclusive, at
most 8 plotted series and at most 2 of type "area".

Required columns - charts: `chart_id, title_sl, title_en`; chart_series:
`chart_id, position, alias, source_type, series_code, formula, plot, legend_sl, legend_en, type, growth, index, rolling`.
Other columns are ignored.
