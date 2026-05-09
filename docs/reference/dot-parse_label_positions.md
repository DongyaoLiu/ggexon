# Parse colon-delimited label_direction string into ordered position vector

Splits a label_direction string like `"bottom:top:center"` into
`c("bottom", "top", "center")`. Valid tokens are `"top"`, `"bottom"`,
and `"center"`.

## Usage

``` r
.parse_label_positions(label_direction)
```

## Arguments

- label_direction:

  Character string, possibly colon-delimited.

## Value

Character vector of valid position tokens in the order they appear in
the input.
