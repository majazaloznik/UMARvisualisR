# Draw vertical marker lines

Black dashed verticals at the given dates, drawn under the series like
[draw_forecast](https://majazaloznik.github.io/UMARvisualisR/reference/draw_forecast.md).
On bar charts the x axis is bar indices rather than dates, so positions
come from
[interpolate_x](https://majazaloznik.github.io/UMARvisualisR/reference/interpolate_x.md)
against the bar midpoints.

## Usage

``` r
draw_vlines(vline, x_values = NULL)
```

## Arguments

- vline:

  Date vector, or NULL

- x_values:

  for bar charts, the list returned by
  [base_barplot](https://majazaloznik.github.io/UMARvisualisR/reference/base_barplot.md);
  NULL for line and area charts

## Value

nothing, draws on the current device
