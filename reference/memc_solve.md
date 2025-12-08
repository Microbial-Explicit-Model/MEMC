# Solve a MEMC configuration

Solve a MEMC configuration

## Usage

``` r
memc_solve(mod, time, params = NULL, state = NULL, ...)
```

## Arguments

- mod:

  model object, for example one of the list entries returned by
  [`memc_all_configs`](https://microbial-explicit-model.github.io/MEMC/reference/memc_all_configs.md)

- time:

  daily time steps for model run

- params:

  default set to NULL, will then use the parameter table read in with
  the `mod` object

- state:

  default set to NULL, will then use the state read read in with the
  `mod` object

- ...:

  additional arguments passed to
  [`ode`](https://rdrr.io/pkg/deSolve/man/ode.html)

## Value

A long-formatted
[`data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)
of the simulation results, with columns:

- time:

  Time point, days

- variable:

  Name of model carbon pool

- value:

  Value of the pool

- units:

  Units of the pool value

- name:

  Model name used

## See also

[`memc_configure`](https://microbial-explicit-model.github.io/MEMC/reference/memc_configure.md)

## Examples

``` r
out <- memc_solve(MEND_config, time = 0:10)
plot(out)
```
