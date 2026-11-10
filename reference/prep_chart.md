# Prepare a chart configuration from a dataframe

Takes a dataframe in wide or long format and prepares a chart
configuration object that can be passed to
[view_chart](https://majazaloznik.github.io/UMARvisualisR/reference/view_chart.md),
[save_chart](https://majazaloznik.github.io/UMARvisualisR/reference/save_chart.md),
or
[get_code](https://majazaloznik.github.io/UMARvisualisR/reference/get_code.md).

## Usage

``` r
prep_chart(
  data,
  type = "line",
  title = NULL,
  y_axis = NULL,
  legend = NULL,
  colours = NULL,
  dashed = NULL,
  dotted = NULL,
  xmin = NULL,
  xmax = NULL,
  stacked = FALSE,
  emphasis = NULL,
  legend_columns = 2,
  rolling = NULL,
  growth = NULL,
  index = NULL,
  ylim = NULL,
  note = NULL,
  forecast = NULL,
  language = "si",
  axis = NULL,
  y_axis2 = NULL,
  ylim2 = NULL,
  y_ref = NULL,
  y_ref2 = NULL,
  vline = NULL
)
```

## Arguments

- data:

  dataframe in wide or long format

- type:

  character, either a single value applied to all series or a vector of
  per-series values. Valid: "line", "bar" or "area" (for max 2 series).
  Defaults to "line".

- title:

  character, chart title. Defaults to NULL (no title).

- y_axis:

  character, y-axis label. Defaults to NULL.

- legend:

  character vector of legend labels. Defaults to column names (wide) or
  unique group values (long).

- colours:

  integer vector (indices into UMAR palette) or character vector (hex
  colours). Defaults to sequential palette colours.

- dashed:

  integer vector for which series should be drawn dashed

- dotted:

  integer vector for which series should be drawn dotted

- xmin:

  Date or character coercible to Date. Left x-axis limit.

- xmax:

  Date or character coercible to Date. Right x-axis limit.

- stacked:

  logical, stack bar series. Defaults to FALSE.

- emphasis:

  numeric value(s) for emphasised horizontal gridlines. NULL (default)
  auto-detects: 0 if in range, 100 if y_axis contains "indeks"/"index".
  FALSE disables. Numeric vector for explicit values. On a dual-axis
  chart these are read against the left axis.

- legend_columns:

  integer, number of columns in the legend. Defaults to 2.

- rolling:

  number of periods to calculate rolling mean over

- growth:

  "YOY", "MOM" or "QOQ"

- index:

  base_period either a year in YYYY format or quarter in 2023Q2 format
  or month in 2023M03 format

- ylim:

  numeric(2) manual y-axis limits, e.g. c(90, 120). NULL (default)
  auto-detects. Ignored for bar charts (must include 0).

- note:

  character string, possibly with \n breaks

- forecast:

  character or date vector of lenght 2 to determine extend of gray
  background shading.

- language:

  language option "si" or "en", defaulting to the former

- axis:

  integer vector assigning series to the left (1) or right (2) y axis.
  Length 1 or one per series. Defaults to all on the left.

- y_axis2:

  character, right y-axis label. Only used when some series are on axis
  2.

- ylim2:

  numeric(2) manual limits for the right axis. NULL (default) derives
  them from the data, paired with the left axis so that the gridlines
  serve both.

- y_ref:

  reference for pairing y-axis

- y_ref2:

  reference for pairing y-axis

- vline:

  location of vertical line(s)

## Value

An object of class "umar_chart" containing the data and config.

## Details

Wide format: first column is date (Date class), remaining columns are
numeric series. Column names become legend labels by default.

Long format: exactly three columns — one Date, one numeric (values), one
character or factor (series names) or multiple character or factor which
get concatented into series names unless legend in given.
