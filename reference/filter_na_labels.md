# Filter NA labels and positions

Taking the equal lenght label and position vector, removes the indices
in both vectors where the labels are NA.

## Usage

``` r
filter_na_labels(x_positions, x_labels)
```

## Arguments

- x_positions:

  vector of label positions

- x_labels:

  vector of labels

## Value

list with both filtered vectors
