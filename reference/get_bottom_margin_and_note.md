# Helper to get the bottom margin and wrapped note lines

Uses a blind plot to measure the wrapped note text at 9pt, returns the
bottom margin in lines and the wrapped lines for rendering.

## Usage

``` r
get_bottom_margin_and_note(
  note,
  note_ps = ps_note(),
  note_offset = 1,
  note_lead = 0.6
)
```

## Arguments

- note:

  character string, possibly with \n breaks

- note_ps:

  font size for note, defaults 8

- note_offset:

  offset for note

- note_lead:

  lead for note

## Value

list with bottom margin in lines and wrapped note lines
