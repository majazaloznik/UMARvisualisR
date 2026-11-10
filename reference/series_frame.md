# One series in the shape prep_chart's transforms expect

Routes through the same parse step
[prep_chart](https://majazaloznik.github.io/UMARvisualisR/reference/prep_chart.md)
uses (`convert_period_column` then `parse_wide`) so the frame handed to
`transform_*` is identical to what they see there.

## Usage

``` r
series_frame(period_id, value)
```

## Arguments

- period_id:

  character or Date vector

- value:

  numeric vector

## Value

a single-series datapoints frame (Date column + value column)
