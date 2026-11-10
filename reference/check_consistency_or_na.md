# Check if a column has values and they are all the same

Column check that returns true if all valid values are the same. This
means all NAs or some NAs are also OK, basically saying the column has a
single unique valid value, which can be used as a parameter downstream

## Usage

``` r
check_consistency_or_na(column)
```

## Arguments

- column:

  a dataframe column

## Value

any(duplicated(
