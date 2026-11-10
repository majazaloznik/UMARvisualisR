# One axis's slice of the config

Shallow copy with only that axis's series, and for the right axis the
second label moved into `y_axis_label` — so the same label-width code
serves both sides. Same trick
[split_by_unit](https://majazaloznik.github.io/UMARvisualisR/reference/split_by_unit.md)
uses.

## Usage

``` r
config_for_axis(config, which = 1L)
```

## Arguments

- config:

  internal config list

- which:

  1 or 2

## Value

config list
