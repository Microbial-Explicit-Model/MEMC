# Fit a MEMC model to a comparison data

Fit a MEMC model to a comparison data

## Usage

``` r
memc_modfit(config, x, comp_data, lower = -Inf, upper = Inf, ...)
```

## Arguments

- config:

  MEMC model configuration object, either one of the pre-built
  configurations listed in `memc_all_configs` or created using
  `memc_configure`

- x:

  MEMC model parameters or initial conditions that will be fit to the
  data, users will need to provide an initial guess for these values.

- comp_data:

  data frame containing the comparison data that the model will be fit
  this data frame must contain a column for time, the other columns must
  be named for the MEMC model variables.

- lower:

  lower bounds on the parameters; if unbounded set equal to -Inf

- upper:

  bounds on the parameters; if unbounded set equal to Inf

- ...:

  addition arguments that may be passed to FME::modFit

## Value

Results from `FME`::[`modFit`](https://rdrr.io/pkg/FME/man/modFit.html).
