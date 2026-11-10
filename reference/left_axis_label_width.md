# Prepare left axis labels and get width

Changes euros to millions of euros if necessary and calculates how much
space the left margin needs to be. Returns axis title, axis labels, and
number of lines

## Usage

``` r
left_axis_label_width(config, y_axis, language = "si", label_ps = ps_legend())
```

## Arguments

- config:

  config dictionary list from
  [prep_config](https://majazaloznik.github.io/UMARvisualisR/reference/prep_config.md)

- y_axis:

  output of
  [find_pretty_ylim](https://majazaloznik.github.io/UMARvisualisR/reference/find_pretty_ylim.md)

- language:

  language, options en and si, defaulg si

- label_ps:

  size of label font

## Value

list of axis title, axis labels, and number of lines
