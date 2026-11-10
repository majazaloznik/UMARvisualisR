# Helper funciton to get the number of lines for the top margin and title position

A bit of an eclectic function, but there's no way around it. This
funciton creates a blind plot to get the user dimensions of the final
chart, which lets it wrap the title, which lets it get the number of
lines for the title, which along with the number of lines in the legend
and a few graphical parameters lets us know what the top margin has to
be. Also returns the position of the title in lines and the wrapped
title.

## Usage

``` r
get_top_margin_and_title(
  config,
  title_ps,
  legend_lead = 0.7,
  legend_offset = 0.25,
  title_gap = 0.3,
  title_lead = NULL
)
```

## Arguments

- config:

  dictionary list from
  [prep_config](https://majazaloznik.github.io/UMARvisualisR/reference/prep_config.md)

- title_ps:

  title font size in points defaults 10

- legend_lead:

  controls spacing between legend entries

- legend_offset:

  controls spacing to plot area

- title_gap:

  controls spacing from title to plot area

- title_lead:

  controls spacing between title lines

## Value

top margin in lines, vertical title position in lines and wrapped title
