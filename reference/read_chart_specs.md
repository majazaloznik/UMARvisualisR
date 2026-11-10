# Read, type, fill and validate the two chart spec CSVs

The single entry point the driver should use. Reads both files via
[read_spec_csv](https://majazaloznik.github.io/UMARvisualisR/reference/read_spec_csv.md)
(UTF-8 enforced, delimiter sniffed, everything as character), coerces
the columns with known types (`plot`, `dashed`, `dotted`, `stacked` to
logical; `position`, `rolling`, `ylim_*` to numeric; `legend_columns` to
integer, likewise `legend_columns_sl`/`_en` — dates stay character for
[parse_spec_date](https://majazaloznik.github.io/UMARvisualisR/reference/parse_spec_date.md)),
fills default aliases and, unless `validate = FALSE`, runs
[check_chart_specs](https://majazaloznik.github.io/UMARvisualisR/reference/check_chart_specs.md).

## Usage

``` r
read_chart_specs(charts_path, series_path, encoding = "UTF-8", validate = TRUE)
```

## Arguments

- charts_path:

  path to charts.csv

- series_path:

  path to chart_series.csv

- encoding:

  file encoding, default UTF-8

- validate:

  run
  [check_chart_specs](https://majazaloznik.github.io/UMARvisualisR/reference/check_chart_specs.md)
  (fail loud) before returning

## Value

list with `charts` and `chart_series` tibbles
