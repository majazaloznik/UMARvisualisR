# Draw area or ribbon shading

Draws filled polygons for series with type = "area". One area series
shades from baseline (0 by default, 100 for index charts) to the series
values. Two area series shade the region between them.

## Usage

``` r
draw_areas(datapoints, config, y_axis_label)
```

## Arguments

- datapoints:

  list of date+value dataframes

- config:

  internal config with series info

- y_axis_label:

  character y-axis label for baseline detection
