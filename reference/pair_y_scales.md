# Two y scales that share their gridlines

Returns one scale per axis with the same number of intervals, so the
horizontal gridlines serve both. When both sides straddle zero, the zero
line is forced onto the same gridline. Either side can be fixed with
`ylim`/`ylim2`; the other is then derived to match it.

## Usage

``` r
pair_y_scales(
  values_left,
  values_right,
  ylim = NULL,
  ylim2 = NULL,
  ref = 0,
  ref2 = 0,
  n_range = 3:7
)
```

## Arguments

- values_left, values_right:

  numeric vectors of the data on each axis

- ylim, ylim2:

  numeric(2) manual limits, or NULL to derive

- ref:

  reference (usually 0 or 100)

- ref2:

  reference (usually 0 or 100)

- n_range:

  interval counts to consider

## Value

list(left, right, problems), each side shaped like
[find_pretty_ylim](https://majazaloznik.github.io/UMARvisualisR/reference/find_pretty_ylim.md)
(ylim, y_breaks)
