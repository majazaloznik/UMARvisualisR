# Are two y scales compatible?

Compatible means the gridlines can serve both: equal break counts, and —
when both straddle zero — zero at the same height on both, on a
gridline.

## Usage

``` r
check_axis_pair(left, right, ref = 0, ref2 = 0, tol = 1e-08)
```

## Arguments

- left, right:

  scales shaped like
  [find_pretty_ylim](https://majazaloznik.github.io/UMARvisualisR/reference/find_pretty_ylim.md)
  output

- ref:

  reference (usually 0 or 100)

- ref2:

  reference (usually 0 or 100)

- tol:

  relative tolerance

## Value

character vector of problems, empty when the pair is fine
