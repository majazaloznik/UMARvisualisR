# The value an axis is anchored and emphasised on

Index axes are anchored on 100, everything else on 0. Three places used
to decide this independently —
[empty_plot](https://majazaloznik.github.io/UMARvisualisR/reference/empty_plot.md)
with a case-sensitive `"nde(ks|x)"`,
[draw_emphasis](https://majazaloznik.github.io/UMARvisualisR/reference/draw_emphasis.md)
with a case-insensitive `"indeks|index"`, and
[base_barplot](https://majazaloznik.github.io/UMARvisualisR/reference/base_barplot.md)
with a Slovenian-only `"ndeks"` — so an English "Index" label got the
100 line on a line chart and not on a bar chart. They should all call
this.

## Usage

``` r
axis_reference(label)
```

## Arguments

- label:

  axis label, or NULL

## Value

100 for an index axis, otherwise 0
