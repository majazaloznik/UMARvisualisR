# Get x axis parameters

Takes the datapoints and config (for user xmin and xmax mainly) and
calculates the ticmkark positions, the label positions, the default
labels (not handling overlapping just yet) and the xlims for the x axis.

## Usage

``` r
x_axis_label_params(
  datapoints,
  config,
  tickmarks,
  x_lims,
  bar,
  x_values,
  language = "si",
  interval_type = "annual"
)
```

## Arguments

- datapoints:

  list of dataframes from
  [prep_data](https://majazaloznik.github.io/UMARvisualisR/reference/prep_data.md)

- config:

  dictionary list from
  [prep_config](https://majazaloznik.github.io/UMARvisualisR/reference/prep_config.md)

- tickmarks:

  vector from from
  [x_axis_lims_tickmarks](https://majazaloznik.github.io/UMARvisualisR/reference/x_axis_lims_tickmarks.md)

- x_lims:

  vector from from
  [x_axis_lims_tickmarks](https://majazaloznik.github.io/UMARvisualisR/reference/x_axis_lims_tickmarks.md)

- bar:

  logical indicating it's a barplot

- x_values:

  midpoints and params for interpolating

- language:

  "si" or "en"z

- interval_type:

  \- annual (default) or monthly, from
  [x_axis_lims_tickmarks](https://majazaloznik.github.io/UMARvisualisR/reference/x_axis_lims_tickmarks.md)

## Value

list of named x_positions, x_labels, tickmars and x_lims

## Details

Taking into account if the data is only annual - in which case the
tickmarks and labels align - or monthly/quarterly in which case the
labels are between. Taking into account if the last year is complete or
not, which determines the last tickmark and label etc.
