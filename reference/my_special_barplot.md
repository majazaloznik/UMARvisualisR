# Barplot that can handle negative values

A rework of the base barplot funciton that allows for negative values to
be properly plotted i.e. from zero downwards. SOme extra lines to handle
the fact that you might want to pass the add argument to it as well.
Does not play nice with panel.first, but i don't need it to really, so i
left that to the reader as an exercise :)

## Usage

``` r
my_special_barplot(data_matrix, ...)
```

## Arguments

- data_matrix:

  vector or matrix of heights of the bars

- ...:

  to be passed to barplot

## Value

nothing, just plots
