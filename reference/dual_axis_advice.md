# Things worth thinking twice about on a dual-axis chart

Advice rather than errors: every one of these can be the right thing to
do on purpose, and none of them is safe to do by accident, so
[view_chart](https://majazaloznik.github.io/UMARvisualisR/reference/view_chart.md)
emits each as a warning and draws the chart anyway. Emitted from the
renderer rather than
[prep_chart](https://majazaloznik.github.io/UMARvisualisR/reference/prep_chart.md)
because that is where the two scales are known, and because both entry
points converge there — nothing escapes by going through the CSV
pipeline.

## Usage

``` r
dual_axis_advice(
  values_left,
  values_right,
  label_left,
  label_right,
  types_left,
  types_right,
  scales,
  ref = 0,
  ref2 = 0,
  min_share = 0.3
)
```

## Arguments

- values_left, values_right:

  numeric vectors of the data on each axis

- label_left, label_right:

  the two axis labels, or NULL

- types_left, types_right:

  series types ("line", "bar", "area") on each axis

- scales:

  output of
  [pair_y_scales](https://majazaloznik.github.io/UMARvisualisR/reference/pair_y_scales.md)

- ref, ref2:

  reference value for each axis — 0, or 100 for an index

- min_share:

  how much of the combined range each series must still cover before a
  single axis is judged to have been enough. At the default 0.3 this
  fires only when a shared axis would clearly have worked; around 0.15
  it also catches a second axis used to magnify a nearly flat series, at
  the cost of more false positives.

## Value

character vector of advice, empty when nothing looks suspect

## Details

Four things are checked. Whether a single axis would have served both
series, judged by how much of their combined range each one would still
cover — this is the one that catches a second scale added for emphasis
rather than necessity, and it is worded more sharply when the two axis
labels are identical. Whether there are bars on both sides, which cannot
be compared by eye at two different scales. Whether exactly one axis
crosses its reference value, which leaves the emphasised gridline
meaningful for one series and arbitrary for the other. And whatever
[check_axis_pair](https://majazaloznik.github.io/UMARvisualisR/reference/check_axis_pair.md)
reported about the two scales not lining up.

Deliberately not checked: the ratio of the two spans. Two series can
have similar spreads at completely different levels — an index around
100 and a growth rate around zero — where a shared axis would squash
both, so span similarity flags good charts as bad. `min_share` measures
the thing that actually decides it.
