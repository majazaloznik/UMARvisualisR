# Convert period string column to Date

Detects character columns with period patterns (2023M01, 2023Q1, 2023)
and converts them to Date class in-place.

## Usage

``` r
convert_period_column(data)
```

## Arguments

- data:

  dataframe

## Value

dataframe with period column converted to Date (if found)
