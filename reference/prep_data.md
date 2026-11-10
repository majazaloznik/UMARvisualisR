# Whole data prep pipeline for plotting publicaiton ready charts

takes as its input a dataframe with one series per row and all the
relevant info. (see vignette that hasn't been written yet). Checks the
data input is OK, prepares the config, gets the data, transforms the
data if necessary and returns a list od dataframes with all the
datapoints and the config list.

## Usage

``` r
prep_data(df, con, schema = "platform")
```

## Arguments

- df:

  input dataframe with at least the following columns: serija, enota

- con:

  database connection

- schema:

  character schema name, default is "platform"

## Value

list of dataframes and config list
