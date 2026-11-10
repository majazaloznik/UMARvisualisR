# Draw lines onto plot

This works for either plotting only lines on a preexisting empty plot
from
[empty_plot](https://majazaloznik.github.io/UMARvisualisR/reference/empty_plot.md),
or plotting on top of existing barplot from
[base_barplot](https://majazaloznik.github.io/UMARvisualisR/reference/base_barplot.md)
in which case you have to pass it the bar midpoints as x_values

## Usage

``` r
draw_lines(datapoints, config, x_values = NULL)
```

## Arguments

- datapoints:

  list of dataframes from
  [prep_data](https://majazaloznik.github.io/UMARvisualisR/reference/prep_data.md)

- config:

  config dictionary list from
  [prep_config](https://majazaloznik.github.io/UMARvisualisR/reference/prep_config.md)

- x_values:

  vector of midpoints from
  [base_barplot](https://majazaloznik.github.io/UMARvisualisR/reference/base_barplot.md)

## Value

nothing, just draws lines
