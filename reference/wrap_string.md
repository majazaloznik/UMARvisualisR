# Wrap a character string to a specified character width

Uses strwrap to split a string into subsets that are shorter than the
specified width and inserts newlines in between them. Inspired by this
\[SO
answer\](https://stackoverflow.com/questions/7367138/text-wrap-for-plot-titles/7367534#7367534).
Also returns the number of rows it has produced, and an optional max_row
parameter lets you set a threshold for the number of rows after which
the text gets truncated.

## Usage

``` r
wrap_string(string, width = 100, max_rows = 3)
```

## Arguments

- string:

  a character string to be wrapped- vector of length one

- width:

  numeric - desired character width, defaults to 100

- max_rows:

  numeric maximum number of rows allowed in the output, defaults to 3

## Value

a list with the first element a character string with new lines if
required, and the second one a numeric value for how many rows are in
the returned string.
