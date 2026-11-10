# Get max period id from list of dataframes with with periods and their ids

Helper function to get the period id for the most recent data point in a
list of dataframes which have period (date) and period_id columns (i.e.
2023M03)

## Usage

``` r
get_max_period(data_points)
```

## Arguments

- data_points:

  list of dataframes with period and period_id columns

## Value

value of the most recent period_id in list
