# Read one spec CSV with the pipeline's conventions

Delimiter is sniffed from the header line (`;` is Excel-in-a-
European-locale output; `,` decimals in numeric columns are handled by
[coerce_spec_types](https://majazaloznik.github.io/UMARvisualisR/reference/coerce_spec_types.md)).
Every column is read as character (so `readr` never guesses a type) and
the file must be valid UTF-8 — plain "CSV" from Excel on Windows is the
system codepage and fails here with instructions, rather than later
inside a regex.

## Usage

``` r
read_spec_csv(path, encoding = "UTF-8")
```

## Arguments

- path:

  file path

- encoding:

  expected file encoding; keep UTF-8 unless you have a reason

## Value

tibble, all character columns
