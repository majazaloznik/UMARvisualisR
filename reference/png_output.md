# Output to png

Thin wrapper on
[chart_device](https://majazaloznik.github.io/UMARvisualisR/reference/chart_device.md).
Opens device to output to png with filename and size. Afterwards you
need to call dev.off().

## Usage

``` r
png_output(filename, size = "normal")
```

## Arguments

- filename:

  name of file with path

- size:

  small, normal or large, defaults to normal

## Value

nothing, produces png file
