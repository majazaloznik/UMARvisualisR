# Build the wide data table for one chart from its spec rows

Blank aliases are filled via
[fill_default_aliases](https://majazaloznik.github.io/UMARvisualisR/reference/fill_default_aliases.md)
first. Then, in position order, each row is fetched (via `fetch_fn`) or
evaluated (its `formula`, against the table so far), and that row's
`rolling`/`growth`/`index` are applied immediately using the same
functions
[prep_chart](https://majazaloznik.github.io/UMARvisualisR/reference/prep_chart.md)
uses. So a formula sees earlier rows' *final* values, the same series
raw and smoothed are two different columns, and the table is what gets
plotted — pass `NULL` for `rolling`/`growth`/`index` to
[prep_chart](https://majazaloznik.github.io/UMARvisualisR/reference/prep_chart.md)
afterwards.

## Usage

``` r
build_chart_data(
  series_rows,
  con,
  fetch_fn,
  date_valid = NULL,
  schema = "platform"
)
```

## Arguments

- series_rows:

  chart_series rows for exactly one chart_id

- con:

  Database connection object, passed to `fetch_fn`

- fetch_fn:

  `function(codes, con, date_valid, schema)` returning a dataframe with
  `period_id` plus one column per `names(codes)`. In production pass
  [`UMARaccessR::get_series_table`](https://rdrr.io/pkg/UMARaccessR/man/get_series_table.html);
  kept as an argument so UMARvisualisR stays DB-agnostic and tests can
  pass a stub.

- date_valid:

  passed to `fetch_fn`

- schema:

  passed to `fetch_fn`

## Value

A dataframe with `period_id` and one column per alias, in position
order.

## Details

Returns every alias, plotted or not, columns in position order. Subset
to `plot == TRUE` rows before
[prep_chart](https://majazaloznik.github.io/UMARvisualisR/reference/prep_chart.md);
keep the full table for the data export so computed series are auditable
against their inputs.
