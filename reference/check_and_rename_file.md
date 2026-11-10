# Checks if a file is open/locked and renames it

If the file you are trying to write to is open, it is also locked for
writing. So in this case we rename it by adding a timestamp before the
extension. Kludgy but works.

## Usage

``` r
check_and_rename_file(filename)
```

## Arguments

- filename:

  file to check

## Value

(updated) filename
