# Prepare plot configuration from dataframe

Takes the input dataframe which may or may not have config data for the
plot and the series and should have gone through the
[check_plot_inputs](https://majazaloznik.github.io/UMARvisualisR/reference/check_plot_inputs.md)
check first! Updates the default config with whatever is in the
dataframe. Defaults are hardcoded here and the prep will work even if
only the series codes are given in the dataframe, which is the bare
minimum and will produce a basic line plot configuration. The output
config is the input to the plotting function.

## Usage

``` r
prep_config(df)
```

## Arguments

- df:

  with at least the columns series_code and unit

## Value

a config list for plotting
