# Wrap a label to fit a length, breaking on spaces

Greedy word wrap measured with
[`strwidth()`](https://rdrr.io/r/graphics/strwidth.html) at the current
`par("ps")`, so call it under the same point size the label will be
drawn at. Used for the y-axis title, where the limit is the plot height.
Existing line breaks are kept: each `\n`-separated line is wrapped on
its own, so a manual break in the spec still wins.

## Usage

``` r
wrap_to_height(label, max_in)
```

## Arguments

- label:

  character scalar, possibly containing `\n`

- max_in:

  maximum line length in inches

## Value

character scalar with `\n` between wrapped lines
