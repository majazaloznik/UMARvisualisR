# Fill blank aliases from the series code (db rows) or formula (computed rows)

Idempotent. Rows with neither (a db row with no code, a computed row
with no formula) are left blank for
[validate_chart_specs](https://majazaloznik.github.io/UMARvisualisR/reference/validate_chart_specs.md)
to report. Applied internally by the validator and
[build_chart_data](https://majazaloznik.github.io/UMARvisualisR/reference/build_chart_data.md);
the driver gets it via
[read_chart_specs](https://majazaloznik.github.io/UMARvisualisR/reference/read_chart_specs.md).

## Usage

``` r
fill_default_aliases(chart_series)
```

## Arguments

- chart_series:

  chart_series table

## Value

chart_series with `alias` filled where derivable
