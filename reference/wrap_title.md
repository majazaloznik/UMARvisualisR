# Helper funciton to wrap title to appropriate width

Had chatgpt write this for me pretty much. does what it says on the box.

## Usage

``` r
wrap_title(title, max_width = NULL, cex = 1, family = umar_font(), font = 1)
```

## Arguments

- title:

  character

- max_width:

  in user units - default is plot width from par("usr")

- cex:

  numeric

- family:

  of fonts.

- font:

  value for font style

## Value

title but wiht line breaks where needed
