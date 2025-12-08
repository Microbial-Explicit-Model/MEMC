# Global sensitivity ranges for a MEMC model

Given a MEM model configuration, estimate the global parameter
sensitivity.

## Usage

``` r
memc_sensrange(config, t, x, parRange, dist, ...)
```

## Arguments

- config:

  a MEMC model configuration object, either one of the pre-built
  configurations listed in `memc_all_configs` or created using
  `memc_configure`

- t:

  vector of the time steps to run the model at

- x:

  vector of the parameters or initial model pool sizes that will be
  varied

- parRange:

  data frame of the min/max parameter values

- dist:

  str for the distribution according to which the parameters will be
  sampled from, options" "unif" (uniformly random samples), "norm",
  (normally distributed random samples), "latin" (latin hypercube
  distribution), and "grid" (parameters arranged on a grid).

- ...:

  additional arguments passed to
  [`sensRange`](https://rdrr.io/pkg/FME/man/sensRange.html)

## Value

The results of
`FME`::[`sensRange`](https://rdrr.io/pkg/FME/man/sensRange.html).

## See also

Other sensitivity:
[`memc_sensfun()`](https://microbial-explicit-model.github.io/MEMC/reference/memc_sensfun.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Test the sensitivity of the MEND output for V_p, K_p, V_m
pars <- c("V_d" = 3.0e+00,"V_p" = 1.4e+01,"V_m" = 2.5e-01)
prange <- data.frame(min = pars - pars * 0.75,
max = pars + pars * 0.75)
t <- floor(seq(0, 365, length.out = 10))
out <- memc_sensrange(config = MEND_config, t = t, x = pars,
parRange = prange, dist = "latin", num = 10)
plot(out)
} # }
```
