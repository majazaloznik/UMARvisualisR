# Create legend

does what it says on the box based on the config data

## Usage

``` r
create_legend(
  config,
  legend_ps,
  language = "si",
  legend_lead = 0.7,
  legend_offset = 0.25
)
```

## Arguments

- config:

  dictionary list from
  [prep_config](https://majazaloznik.github.io/UMARvisualisR/reference/prep_config.md)

- legend_ps:

  legend text size

- language:

  either "si" or "en"

- legend_lead:

  controls spacing between legend entries

- legend_offset:

  controls spacing to plot area

## Value

nothing, just draws the legend
