# New fun to get date from period with three types of alignment

takes year, quarter or month format and returns first ("l"), middle
("c") or last ("r") date, the default is "c"

## Usage

``` r
get_date_from_period(period, position = c("c", "l", "r"))
```

## Arguments

- period:

  one of three types

- position:

  l, c or r, defaults to middle

## Value

date
