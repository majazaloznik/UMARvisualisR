# Find pretty y-axis limits

Finds y/limits that lead to a pretty y-axis. If the range of the values
is on the line, it artificially extends it by one more break.

## Usage

``` r
find_pretty_ylim(values)
```

## Arguments

- values:

  vector of numeric values

## Value

numeric vector of length 2 to be used as y-axis limit
