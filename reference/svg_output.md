# Output to svg

Thin wrapper on
[chart_device](https://majazaloznik.github.io/UMARvisualisR/reference/chart_device.md).
Needs the svglite package. Text is written as text and resolved against
the fonts installed where the file is opened - see
[chart_device](https://majazaloznik.github.io/UMARvisualisR/reference/chart_device.md).

## Usage

``` r
svg_output(filename, size = "normal")
```

## Arguments

- filename:

  name of file with path

- size:

  small, normal or large, defaults to normal

## Value

nothing, produces svg file
