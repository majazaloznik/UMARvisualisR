# Open an output device for a chart

Dispatches to the right device for `format` at the house size and point
size. Afterwards you need to call
[`dev.off()`](https://rdrr.io/r/grDevices/dev.html).

## Usage

``` r
chart_device(filename, format = "pdf", size = "normal")
```

## Arguments

- filename:

  name of file with path

- format:

  one of
  [chart_formats](https://majazaloznik.github.io/UMARvisualisR/reference/chart_formats.md)

- size:

  small, normal or large, defaults to normal

## Value

the filename actually opened (may differ from `filename` if the original
was locked), invisibly
