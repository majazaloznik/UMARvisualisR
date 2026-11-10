# Prepare right axis labels and get width

Mirror of
[left_axis_label_width](https://majazaloznik.github.io/UMARvisualisR/reference/left_axis_label_width.md):
same measurement at the same point size, same title wrapping to the plot
height, but it sets the right margin instead of the left. Must run after
[get_top_margin_and_title](https://majazaloznik.github.io/UMARvisualisR/reference/get_top_margin_and_title.md),
which resets `mar[4]`.

## Usage

``` r
right_axis_label_width(
  config,
  y_axis,
  language = "si",
  edge_pad = 0.35,
  label_ps = ps_legend()
)
```

## Arguments

- config:

  config for the right axis, from
  [config_for_axis](https://majazaloznik.github.io/UMARvisualisR/reference/config_for_axis.md)

- y_axis:

  the right-hand scale from
  [pair_y_scales](https://majazaloznik.github.io/UMARvisualisR/reference/pair_y_scales.md)

- language:

  language, options en and si, default si

- edge_pad:

  for padding right margin

- label_ps:

  controls label font size

## Value

list of axis title, axis labels, positions and number of lines
