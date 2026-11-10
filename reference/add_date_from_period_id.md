# Add period date column from period ID

Converts the 2023M02 type and 2023Q2 into 1.2.2023 or 1.4.2023, while
keeping the year alone.

## Usage

``` r
add_date_from_period_id(df)
```

## Arguments

- df:

  dataframe with a period_id column

## Value

dataframe with an extra column called period with the dates
