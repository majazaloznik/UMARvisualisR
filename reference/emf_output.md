# Output to emf

Thin wrapper on
[chart_device](https://majazaloznik.github.io/UMARvisualisR/reference/chart_device.md).
Needs the devEMF package. EMF is Word's native vector format on Windows.

## Usage

``` r
emf_output(filename, size = "normal")
```

## Arguments

- filename:

  name of file with path

- size:

  small, normal or large, defaults to normal

## Value

nothing, produces emf file
