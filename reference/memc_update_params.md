# Update the parameter table with new values

Update the parameter table with new values

## Usage

``` r
memc_update_params(new_params, param_table)
```

## Arguments

- new_params:

  a named vector of parameter values to update the param table.

- param_table:

  data.table containing the following columns: parameter, value, and
  units, this is the basic parameter table that will be updated with the
  new values.

## Value

Updated
[`data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)
containing the new parameter values.

## See also

Other helper functions:
[`memc_configure()`](https://microbial-explicit-model.github.io/MEMC/reference/memc_configure.md),
[`memc_update_config()`](https://microbial-explicit-model.github.io/MEMC/reference/memc_update_config.md),
[`update_state()`](https://microbial-explicit-model.github.io/MEMC/reference/update_state.md)
