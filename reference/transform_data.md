# Transform a series dataframe with the series confing

Takes the dataframe with date and value columns and the series' config
list (not the chart config list, just the series') and does the
transformations. Currently the default is that rolling averages are
performed first, but i can change that later and add the other way
round. The
[check_plot_inputs](https://majazaloznik.github.io/UMARvisualisR/reference/check_plot_inputs.md)
function has already made sure the combinations of transformations are
all legal.

## Usage

``` r
transform_data(df, series_config)
```

## Arguments

- df:

  dataframe with date and value columns

- series_config:

  list of series' config paramters from
  [prep_config](https://majazaloznik.github.io/UMARvisualisR/reference/prep_config.md).

## Value

df and series' config
