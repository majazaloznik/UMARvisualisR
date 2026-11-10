# Draw forecast shading rectangle

Draws a translucent grey rectangle from `forecast[1]` to `forecast[2]`
spanning the full height of the plot area. Used to indicate periods of
forecast or projected data. For bar charts, the rectangle is drawn
inside
[base_barplot](https://majazaloznik.github.io/UMARvisualisR/reference/base_barplot.md)
instead.

## Usage

``` r
draw_forecast(forecast, bar, x_values = NULL)
```

## Arguments

- forecast:

  Date vector of length 2 (start and end), or NULL to skip.

- bar:

  logical, TRUE if bar chart (skips drawing; bars handle it).

- x_values:

  list with dates and midpoints, for interpolation. NULL for line
  charts.

## Value

nothing, draws to open device
