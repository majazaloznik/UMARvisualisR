# One axis on a nice grid with exactly n intervals

With `j` supplied the axis is anchored so that zero falls exactly on the
j-th break, which is what lets two axes share a zero line. With
`j = NULL` the range floats to whatever covers the data most tightly,
padded up to n intervals — never pushing an all-positive axis below
zero.

## Usage

``` r
axis_on_grid(lo, hi, n, j = NULL, ref = 0)
```

## Arguments

- lo, hi:

  data range to cover

- n:

  number of intervals (there are n + 1 breaks)

- j:

  intervals below zero, or NULL to let the range float

- ref:

  reference line (usually 0 or 100)

## Value

list(ylim, y_breaks, step), or NULL if no nice step fits
