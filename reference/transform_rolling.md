# New function for rolling average transformations

New function for rolling average transformations

## Usage

``` r
transform_rolling(df, periods = 3, align = "r")
```

## Arguments

- df:

  dataframe with date and value columns

- periods:

  number of periods to be averaged over

- align:

  alignment method - center is the default, right is correct and left i
  don't know why it even exists

## Value

dataframe with transformed values.
