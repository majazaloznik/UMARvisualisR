# Default alias derived from a series code or a formula, plus transforms

For a series code: lowercased, runs of non-alphanumerics collapsed to
`_`, trimmed, prefixed with `x_` if it would start with a digit. For a
formula (`formula = TRUE`) the operators are spelled out first so they
survive that step: `+ - * / ^` become
`_plus_ _minus_ _times_ _div_ _pow_`, a leading `-` becomes `neg_`.
Transforms are appended as suffixes in application order: `_roll<n>`,
`_<growth>`, `_idx<base>`.

## Usage

``` r
default_alias(x, growth = NA, index = NA, rolling = NA, formula = FALSE)
```

## Arguments

- x:

  character vector of series codes, or formulas if `formula = TRUE`

- growth, index, rolling:

  the row's transform columns (NA when unused)

- formula:

  logical, is `x` a formula rather than a code

## Value

character vector of aliases matching `^[a-z][a-z0-9_]*$`

## Details

Deterministic, so it survives position edits; two rows collide only if
they show the same thing — same code (or formula) with the same
transforms.
