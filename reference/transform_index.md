# Function for transformation to an index with a specific base period

Calculates the index based on the period. The following options are
allowed:

## Usage

``` r
transform_index(df, base_period)
```

## Arguments

- df:

  data frame with date and value columns

- base_period:

  either a year in YYYY format or quarter in 2023Q2 format or month in
  2023M03 format

## Value

dataframe with trnsformed values

## Details

\+ the base period is a year: the average will be taken of the months or
quarters, of course this works fine for annual data as well. + if the
base period is a quarter, you can only do the transformation on
quarterly data - but this is checked in the check_plot_inputs function +
if the base period is a month, you can also only use it on monthly data.

The format of the base_period is not checked because if is checked in
[check_plot_inputs](https://majazaloznik.github.io/UMARvisualisR/reference/check_plot_inputs.md)
already.

If the base period does not exist in the data, the oldest value is used
and a warning is issued to that effect.
