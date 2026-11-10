# My legend function

My legend function

## Usage

``` r
legend_mz2(
  x = par("usr")[[1]],
  y = par("usr")[[4]],
  legend,
  fill = NULL,
  col = NULL,
  lty = NULL,
  lwd = NULL,
  xjust = 0,
  yjust = 0,
  x.intersp = 0.2,
  y.intersp = 0.8,
  text.col = "black",
  text.font = NULL,
  ncol = 1,
  family = umar_font()
)
```

## Arguments

- x:

  the x and y co-ordinates to be used to position the legend. They can
  be specified by keyword or in any way which is accepted by xy.coords:
  See ‘Details’.

- y:

  the x and y co-ordinates to be used to position the legend. They can
  be specified by keyword or in any way which is accepted by xy.coords:
  See ‘Details’.

- legend:

  a character or expression vector of length ≥ 1 to appear in the
  legend. Other objects will be coerced by as.graphicsAnnot.

- fill:

  if specified, this argument will cause boxes filled with the specified
  colors (or shaded in the specified colors) to appear beside the legend
  text.

- col:

  color of points or lines appearing in the legend.

- lty:

  the line types and widths for lines appearing in the legend. One of
  these two must be specified for line drawing.

- lwd:

  the line types and widths for lines appearing in the legend. One of
  these two must be specified for line drawing.

- xjust:

  how the legend is to be justified relative to the legend x location. A
  value of 0 means left justified, 0.5 means centered and 1 means right
  justified.

- yjust:

  the same as xjust for the legend y location.

- x.intersp:

  character interspacing factor for horizontal (x) spacing.

- y.intersp:

  the same for vertical (y) line distances.

- text.col:

  the color used for the legend text.

- text.font:

  the font used for the legend text, see text.

- ncol:

  the number of columns in which to set the legend items (default is 1,
  a vertical legend).

- family:

  font family

## Value

nothing, draws legend
