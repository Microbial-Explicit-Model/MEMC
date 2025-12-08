# Local sensitivity for a MEMC model

Estimate the local effect of a parameter on a MEMC model output

## Usage

``` r
memc_sensfun(config, t, x, ...)
```

## Arguments

- config:

  a MEMC model configuration object, either one of the pre-built
  configurations listed in `memc_all_configs` or created using
  `memc_configure`

- t:

  vector of the time steps to run the model at

- x:

  vector of the parameters or initial state values to test

- ...:

  additional arguments passed to
  [`sensFun`](https://rdrr.io/pkg/FME/man/sensFun.html)

## Value

The results of
`FME`::[`sensFun`](https://rdrr.io/pkg/FME/man/sensFun.html).

## See also

Other sensitivity:
[`memc_sensrange()`](https://microbial-explicit-model.github.io/MEMC/reference/memc_sensrange.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Test the sensitivity of the MEND output for V_p, K_p, V_m
pars <- c("V_d" = 3.0e+00,"V_p" = 1.4e+01,"V_m" = 2.5e-01)
out <- memc_sensfun(config = MEND_config, t = t, x = pars)
plot(out)
} # }
```
