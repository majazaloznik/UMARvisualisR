# Cut datapoints to x limits from config

takes the maximum range of all the series in the datapoints tables and
then cuts it down by the values of xmin and xmax passed in the config
dictionary. If the intervals don't overlap, uses just the datapoint
range, ignoring the user input.

## Usage

``` r
cut_to_x_range(datapoints, config)
```

## Arguments

- datapoints:

  list of dataframes from
  [prep_data](https://majazaloznik.github.io/UMARvisualisR/reference/prep_data.md)

- config:

  config dictionary list from
  [prep_config](https://majazaloznik.github.io/UMARvisualisR/reference/prep_config.md)

## Value

a list with the xmin and xmax values of the data in date format and the
filtered datapoints

## Details

config\$xmax is always the first day of its period (parse_spec_date's
contract), but center_dates() has already shifted plotted dates to
mid-period before this runs. A literal comparison against config\$xmax
would therefore always exclude the period xmax names. xmax is rounded up
to the last day of its period before being used as a bound; xmin is left
as-is since mid-period dates are always \>= first-of-period.

Returns filtered datapoints.
