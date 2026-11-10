# Blank a series after another series' last observation

Two series plotted together but published on different schedules leave
the faster one apparently running ahead of the story. This blanks `x`
for every period after the last one where `reference` has a value, so
both lines end together.

## Usage

``` r
truncate_to(x, reference)
```

## Arguments

- x:

  numeric vector to truncate

- reference:

  numeric vector whose coverage sets the cutoff

## Value

x, with everything after reference's last observation set to NA

## Details

Meant to be called from a `formula` cell in chart_series.csv, with the
reference series present as its own row with `plot = FALSE` — it then
stays in the table
[build_chart_data](https://majazaloznik.github.io/UMARvisualisR/reference/build_chart_data.md)
returns, so the data export shows what caused the truncation.
Positional, like every formula in a chart spec: it assumes the table is
in period order.
