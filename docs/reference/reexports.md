# Objects imported from other packages

These helpers are re-exported so ggexon users can access them without
attaching the upstream package explicitly.

## Usage

``` r
zeroGrob()

unit(x, units, data = NULL)

arrow(angle = 30, length = unit(0.25, "inches"), ends = "last", type = "open")

alpha(colour, alpha = NA)
```

## Examples

``` r
ggplot(mpg, aes(displ, hwy)) +
  geom_point(alpha = 0.5, colour = "blue")


ggplot(mpg, aes(displ, hwy)) +
  geom_point(colour = alpha("blue", 0.5))
```
