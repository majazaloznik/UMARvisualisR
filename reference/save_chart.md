# Save a chart to file

Renders the chart and saves it. Format is determined by the `format`
argument, or by the file extension if one is provided.

## Usage

``` r
save_chart(chart, filename, format = "svg", size = 2)
```

## Arguments

- chart:

  an object of class "umar_chart" from
  [prep_chart](https://majazaloznik.github.io/UMARvisualisR/reference/prep_chart.md)

- filename:

  character path to output file. If no extension is provided, one is
  added based on `format`.

- format:

  character, one of
  [chart_formats](https://majazaloznik.github.io/UMARvisualisR/reference/chart_formats.md).
  Defaults to "svg". Ignored if `filename` already has a valid
  extension.

- size:

  integer, chart size: 1 (small), 2 (normal), 3 (large). Defaults to 2.

## Value

invisible chart object

## Details

`pdf` and `png` need nothing extra. `svg` needs the svglite package,
`emf` needs devEMF; both are in Suggests.

For insertion into Word: `emf` is Word's native vector format on Windows
and is drawn with text converted to paths, so it does not depend on the
reader having the house font installed. `svg` is also vector and Word
renders it natively, but it carries text as text and will substitute a
different font - and therefore re-flow every label - on a machine
without the house font. `png` is immune to both but does not scale.
