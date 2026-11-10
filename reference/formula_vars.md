# Variable names referenced by a formula string

Variable names referenced by a formula string

## Usage

``` r
formula_vars(formula)
```

## Arguments

- formula:

  character scalar holding an R expression

## Value

character vector of referenced names (empty for a constant), or
`NA_character_` if the string does not parse as a single expression
