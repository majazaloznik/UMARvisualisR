# Plots empty plot with gridlines

Plots an empty plot with the gridlines, emphasised on 100 for indices a
and zero if in range.

## Usage

``` r
empty_plot(x_lims, y_axis, y_axis_label, pad = x_pad("line"))
```

## Arguments

- x_lims:

  output of
  [get_x_lims](https://majazaloznik.github.io/UMARvisualisR/reference/get_x_lims.md)

- y_axis:

  output of
  [find_pretty_ylim](https://majazaloznik.github.io/UMARvisualisR/reference/find_pretty_ylim.md)

- y_axis_label:

  whatever is in config\$y_axis_label - required to check if the unit is
  an index, so the 100 gridline can be emphasised.

- pad:

  numeric or vector of length 2 padding x axis ednes.

## Value

nothing, plots to open device
