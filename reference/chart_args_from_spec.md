# Translate one chart's spec into prep_chart() arguments

Pure mapping from the two spec tables plus the table from
[build_chart_data](https://majazaloznik.github.io/UMARvisualisR/reference/build_chart_data.md)
to a named list you can `do.call(prep_chart, ...)` — or inspect first.
Rules applied here, all mirroring
[prep_chart](https://majazaloznik.github.io/UMARvisualisR/reference/prep_chart.md)'s
own:

- only `plot == TRUE` rows are plotted, in position order;

- `rolling`/`growth`/`index` are NULL (already applied by
  [build_chart_data](https://majazaloznik.github.io/UMARvisualisR/reference/build_chart_data.md));

- blank `y_axis` falls back to
  [default_y_axis](https://majazaloznik.github.io/UMARvisualisR/reference/default_y_axis.md);

- `dashed`/`dotted` logical columns become index vectors;

- the second `area` series gets `NA` legend;

- a literal `\n` in `title`, `y_axis` or `note` becomes a line break;

- `legend_columns_<lang>` is used if present, else `legend_columns`,
  else 2;

- `colour` all-numeric strings become palette indices;

- `xmin`/`xmax`/`forecast_*` go through
  [parse_spec_date](https://majazaloznik.github.io/UMARvisualisR/reference/parse_spec_date.md):
  ISO or period form only.

## Usage

``` r
chart_args_from_spec(chart, series_rows, wide, language = "si")
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

named list of
[prep_chart](https://majazaloznik.github.io/UMARvisualisR/reference/prep_chart.md)
arguments
