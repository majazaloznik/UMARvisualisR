# Apply limits to x-axis range of data

Takes a dataframe with the datapoints and periods and removes the
periods before xmin (default is 1.1.2011) and after xmax (defaults to
max in the data).

## Usage

``` r
apply_xlims(df, xmin = "2011-01-01", xmax = NULL)
```

## Arguments

- df:

  dataframe with period column in Date format

- xmin:

  Date, default "2011-01-01"

- xmax:

  Date, default maximum available in data.

## Value

df as input with possibly fewer rows
