# Stop par mgp warning

convenience funciton to stop annoying warning when using differing signs
in the mgp parameters. For some reason cannot be suppressed using
suppressWarnings, but this works

## Usage

``` r
par_mgp(mgp = c(3, -0.2, 0))
```

## Arguments

- mgp:

  numeric vector of length 3 to set mgp

## Value

nothing, changes mgp parameter
