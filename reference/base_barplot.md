# Base barplot

Can be used as base for combo of bar and line plot as well. All the info
is in the datapoints and config.

## Usage

``` r
base_barplot(datapoints, config, y_axis, forecast = NULL, pad = x_pad("bar"))
```

## Arguments

- datapoints:

  list of dataframes from
  [prep_data](https://majazaloznik.github.io/UMARvisualisR/reference/prep_data.md)

- config:

  config dictionary list from
  [prep_config](https://majazaloznik.github.io/UMARvisualisR/reference/prep_config.md)

- y_axis:

  output of
  [find_pretty_ylim](https://majazaloznik.github.io/UMARvisualisR/reference/find_pretty_ylim.md)

- forecast:

  forecast param

- pad:

  numeric or vector of length 2 for xlim edge padding

## Value

midpoints, of course also plots a barplot to open device
