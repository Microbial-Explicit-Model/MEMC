# Configurations

The various model configurations that are shipped with the MEMC package,
such as: `BAMS_config`, `COMISSION_config`, `CORPSE_config`,
`MEND_config`, `MIMCS_config`, and `MEMS_config`. All supported model
configurations are listed in `memc_all_configs`.

## Details

These model configurations can be used with the `memc_solve` function to
complete the desired model simulations.

## See also

dynamics, fluxes

## Examples

``` r
## Using memc_solve with the MEND_config configuration.
out <- memc_solve(mod = MEND_config, time = 0:10)
```
