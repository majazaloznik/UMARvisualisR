# Map umar_chart object to the internal config format

Bridges from the user-facing structure to the format expected by the
existing rendering functions. Each series carries its axis, and its unit
comes from that axis's label, so
[left_axis_label_width](https://majazaloznik.github.io/UMARvisualisR/reference/left_axis_label_width.md)
and
[right_axis_label_width](https://majazaloznik.github.io/UMARvisualisR/reference/right_axis_label_width.md)
see one unit each rather than both.

## Usage

``` r
to_internal_config(chart)
```

## Arguments

- chart:

  umar_chart object

## Value

config list in internal format
