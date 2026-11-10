# Get the datapoints from the database

taking the configuration list output from
[prep_config](https://majazaloznik.github.io/UMARvisualisR/reference/prep_config.md)
returns a list with a dataframe for each of the series of the correct
vintage

## Usage

``` r
get_data(config, con, schema = "platform")
```

## Arguments

- config:

  configuration list

- con:

  connection to database

- schema:

  character schema name, default is "platform"

## Value

list with a df for each series
