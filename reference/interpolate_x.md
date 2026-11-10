# Linear interpolation for x axis transformation

used to transform a numeric axis to a date axis and scale other date
vectors to the axis coordinates. assumes the input dates and values are
on a line i.e. only takes the first and last values into account

## Usage

``` r
interpolate_x(original_dates, x_values, new_dates)
```

## Arguments

- original_dates:

  date vector same length as x_values

- x_values:

  numeric vector, same length as orinal dates

- new_dates:

  date vector

## Value

interpolated x values of new dates
