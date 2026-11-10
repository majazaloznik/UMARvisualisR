# Apply one row's transforms, in prep_chart's order

rolling -\> growth -\> index, each via the same internal function
[prep_chart](https://majazaloznik.github.io/UMARvisualisR/reference/prep_chart.md)
calls, with its QOQ/MOM frequency check. Row count must be preserved.

## Usage

``` r
apply_row_transforms(period_id, value, rolling, growth, index)
```

## Arguments

- period_id:

  the table's period column

- value:

  numeric vector for this row

- rolling, growth, index:

  the row's transform values (NA when unused)

## Value

list(value = transformed numeric vector, base_period = resolved index
base or NA)
