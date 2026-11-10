# Year label squishing function

Squishes year labels using medium squishing - all but the first label go
to YY - or extra squishing: keeping only every 5th one, plus the first
and last value.

## Usage

``` r
year_squisher(sequence, extra = FALSE)
```

## Arguments

- sequence:

  numeric sequence of YYYY values of lenght \>2

- extra:

  logical indicator of extra squishing

## Value

same length sequence of character labels
