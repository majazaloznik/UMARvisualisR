# Get data values as a vector

Returns the data values as a vector to be used to get the ylims, but
also adds a 0 to the range if the chart is a barchart and also makes
sure the ylims are correct if it's a stacked barchart. Christ, i am
really overdoing this shit.

## Usage

``` r
get_data_values(datapoints, config)
```

## Arguments

- datapoints:

  list of dataframes from
  [prep_data](https://majazaloznik.github.io/UMARvisualisR/reference/prep_data.md)

- config:

  config dictionary list from
  [prep_config](https://majazaloznik.github.io/UMARvisualisR/reference/prep_config.md)

## Value

vector of numeric values
