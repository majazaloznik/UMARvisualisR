# Check input data for multi-line chart

New version for publication ready charts. Helper function to check that
the input data dataframe for multi line charts is correct. And updates
the units if they are missing or if the series have transformations.
This means that series used in a single chart have to: - be unique -
have a single unit - possibly two for dual y-axis - have a maximum of 8
series ...

## Usage

``` r
check_plot_inputs(df, con, schema = "platform")
```

## Arguments

- df:

  input dataframe with at least the following columns: serija, enota

- con:

  database connection

- schema:

  character schema name, default is "platform"

## Value

input df, possibly with updated main titles.
