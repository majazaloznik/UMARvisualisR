# Device point size

Every output device must be opened at this point size. It is not a text
size - no text is drawn at 12 - but it is what fixes `par("csi")` at 0.2
inches, which is the unit for every spacing constant in the package
(`legend_lead`, `note_lead`, `title_lead`, `title_gap`, `legend_offset`,
[axis_label_gap](https://majazaloznik.github.io/UMARvisualisR/reference/axis_label_gap.md)
and all `mar` values). A device opened at a different point size renders
the same chart with silently different spacing.

## Usage

``` r
device_pointsize()
```

## Value

numeric scalar
