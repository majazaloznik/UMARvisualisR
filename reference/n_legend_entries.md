# Number of legend entries that will actually be drawn

Series with an NA legend label get no entry — either deliberately blank
in the spec, or the second of two area series. Charts with a single
series get no legend at all. Both
[get_top_margin_and_title](https://majazaloznik.github.io/UMARvisualisR/reference/get_top_margin_and_title.md)
and
[view_chart](https://majazaloznik.github.io/UMARvisualisR/reference/view_chart.md)
call this, so the top margin reserved for the legend and the legend
actually drawn cannot disagree.

## Usage

``` r
n_legend_entries(config)
```

## Arguments

- config:

  internal config list with a series element

## Value

integer count of legend entries
