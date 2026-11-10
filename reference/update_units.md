# Updates units in publication chart input table

Updates the units based on the transformations - to index or and then
queries the database for the remaining units if there are still any
missing. And fixes the capitalisation ready for the y-label

## Usage

``` r
update_units(df, con, schema = "platform")
```

## Arguments

- df:

  with at a minimum the columns serija, enota, indeks_let and rast

- con:

  database connection

- schema:

  schema name, defaults to platform

## Value

df with updated
