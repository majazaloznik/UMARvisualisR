# Parse a spec date: ISO (YYYY-MM-DD) or period (YYYYMnn / YYYYQn), else stop

Deliberately refuses locale forms like `01/01/2022`:
[`as.Date()`](https://rdrr.io/r/base/as.Date.html) would silently read
that as year 1. Period form maps to the first day of the period. `Date`
input passes through.

## Usage

``` r
parse_spec_date(x, what = "date")
```

## Arguments

- x:

  scalar: character, Date, or blank/NA

- what:

  column name, for the error message

## Value

a Date, or NULL if blank
