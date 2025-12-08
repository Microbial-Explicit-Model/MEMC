# Return a color palette for the the default MEMC model configurations and other simulations

Return a color palette for the the default MEMC model configurations and
other simulations

## Usage

``` r
memc_colorPalette(name = NULL)
```

## Arguments

- name:

  input vector containing the model names to return the color codes for,
  default will return colors for all the model configurations,

## Value

A vector containing color hex codes for the different model
configurations.

## Examples

``` r
if (FALSE) { # \dontrun{
out <- memc_solve(MEND_config, time = 0:100)
ggplot(data = out) +
  geom_line(aes(time, value, color = name)) +
  scale_color_manual(values = memc_colorPalette()) + 
  facet_wrap("variable", scales = "free")
} # }
```
