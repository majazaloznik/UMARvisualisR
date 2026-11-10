# Default y-axis label implied by the plotted series' transforms

Same rule as
[prep_chart](https://majazaloznik.github.io/UMARvisualisR/reference/prep_chart.md):
`"%"` when every series has a growth transform; `"Index (<base> = 100)"`
/ `"Indeks (...)"` when every series is indexed to the same base;
otherwise `NULL`. Pass the *plotted* rows' columns.

## Usage

``` r
default_y_axis(growth, index, language = "si")
```

## Arguments

- growth, index:

  character vectors, one element per plotted series

- language:

  "si" or "en"

## Value

character scalar or NULL
