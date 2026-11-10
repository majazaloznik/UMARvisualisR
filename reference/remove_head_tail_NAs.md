# Remove NA rows at start and end

Removes rows containing NAs in the column \`value\`(or all the columns
except for \`period\` to be more precise), but only at the beginning and
end of the table.

## Usage

``` r
remove_head_tail_NAs(df)
```

## Arguments

- df:

  data frame with at least period and value columns

## Value

dataframe with possibly fewer rows than going in.
