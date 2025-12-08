# The MEMC model configuration for MEMS

The MEMS configuration is based off of Robertson et al. 2019. This model
uses LM for the DOM uptake, LM for POM decomposition, and LM for MB
decay; see
[dynamics](https://microbial-explicit-model.github.io/MEMC/reference/dynamics.md)
for more details.

## Usage

``` r
MEMS_config
```

## Format

An object created from
[`memc_configure`](https://microbial-explicit-model.github.io/MEMC/reference/memc_configure.md),
containing the following elements.

- name:

  MEMS

- table:

  table of the dynamics used by this model configuration; see
  [dynamics](https://microbial-explicit-model.github.io/MEMC/reference/dynamics.md)
  for more details.

- params:

  data table of the model parameters; see
  [memc_params](https://microbial-explicit-model.github.io/MEMC/reference/memc_params.md)
  for more details.

- state:

  vector of the initial state values; see
  [memc_initial_state](https://microbial-explicit-model.github.io/MEMC/reference/memc_initial_state.md)
  for more details.

## References

Robertson et al.: Unifying soil organic matter formation and persistence
frameworks: the MEMS model, Biogeosciences, 16, 1225–1248, 2019.
<https://doi.org/10.5194/bg-16-1225-2019>

## See also

Other configurations:
[`BAMS_config`](https://microbial-explicit-model.github.io/MEMC/reference/BAMS_config.md),
[`COMISSION_config`](https://microbial-explicit-model.github.io/MEMC/reference/COMISSION_config.md),
[`CORPSE_config`](https://microbial-explicit-model.github.io/MEMC/reference/CORPSE_config.md),
[`MEND_config`](https://microbial-explicit-model.github.io/MEMC/reference/MEND_config.md),
[`MIMCS_config`](https://microbial-explicit-model.github.io/MEMC/reference/MIMCS_config.md)

## Examples

``` r
## Use memc_solve to run the simulation using this configuration.
print(MEMS_config)
#> $name
#> [1] "MEMS"
#> 
#> $table
#>   model DOMuptake POMdecomp MBdecay
#> 1  MEMS        LM        LM      LM
#> 
#> $params
#>    parameter                                              description
#> 1        V_p        maximum specific decomposition rate for POM by EP
#> 2        K_p        half-saturation constant for decomposition of POM
#> 3        V_m        maximum specific decomposition rate for MOM by EM
#> 4        K_m        half-saturation constant for decomposition of MOM
#> 5        V_d       maximum specific uptake rate of D for growth of MB
#> 6        K_d half-saturation constant of uptake of D for growth of MB
#> 7        f_d                fraction of decomposed P allocated to DOM
#> 8        g_d                      fraction of dead B allocated to DOM
#> 9       p_ep                      fraction of mR for production of EP
#> 10      p_em                      fraction of mR for production of EM
#> 11      r_ep                                      turnover rate of EP
#> 12      r_em                                      turnover rate of EM
#> 13     Q_max                            maximum DOC sorption capacity
#> 14     K_ads                                 specific adsorption rate
#> 15     K_des                                          desorption rate
#> 16   dd_beta            strength of density dependent microbial decay
#> 17 Input_POM                                               POM input 
#> 18 Input_DOM                                                DOM input
#> 19       CUE                                    carbon use efficiency
#>              units   value
#> 1  mgC mgC^-1 d^-1  14.000
#> 2     mgC / g soil  50.000
#> 3  mgC mgC^-1 d^-1   0.250
#> 4      mg C/g soil 250.000
#> 5  mgC mgC^-1 d^-1   3.000
#> 6      mg C/g soil   0.250
#> 7             <NA>   0.500
#> 8             <NA>   0.500
#> 9             <NA>   0.010
#> 10            <NA>   0.010
#> 11 mgC mgC^-1 d^-1   0.001
#> 12 mgC mgC^-1 d^-1   0.001
#> 13    mgC / g soil   3.400
#> 14 mgC mgC^-1 d^-1   0.006
#> 15 mgC mgC^-1 d^-1   0.001
#> 16            <NA>   1.000
#> 17            mg C   0.000
#> 18            mg C   0.000
#> 19                   0.400
#> 
#> $state
#>      POM      MOM      QOM       MB      DOM       EP       EM       IC 
#> 10.00000  5.00000  0.10000  2.00000  1.00000  0.00001  0.00001  0.00000 
#>      Tot 
#> 18.10002 
#> 
memc_solve(mod = MEMS_config, time = 1:10)
#>      time variable        value       units   name
#>     <num>   <fctr>        <num>      <char> <char>
#>  1:     1      POM 1.000000e+01 mg C/g soil   MEMS
#>  2:     2      POM 8.818304e-02 mg C/g soil   MEMS
#>  3:     3      POM 4.485527e-02 mg C/g soil   MEMS
#>  4:     4      POM 2.175292e-02 mg C/g soil   MEMS
#>  5:     5      POM 1.052347e-02 mg C/g soil   MEMS
#>  6:     6      POM 5.094580e-03 mg C/g soil   MEMS
#>  7:     7      POM 2.470695e-03 mg C/g soil   MEMS
#>  8:     8      POM 1.202598e-03 mg C/g soil   MEMS
#>  9:     9      POM 5.904516e-04 mg C/g soil   MEMS
#> 10:    10      POM 2.932550e-04 mg C/g soil   MEMS
#> 11:     1      MOM 5.000000e+00 mg C/g soil   MEMS
#> 12:     2      MOM 1.063741e+01 mg C/g soil   MEMS
#> 13:     3      MOM 1.109063e+01 mg C/g soil   MEMS
#> 14:     4      MOM 1.131369e+01 mg C/g soil   MEMS
#> 15:     5      MOM 1.142133e+01 mg C/g soil   MEMS
#> 16:     6      MOM 1.147306e+01 mg C/g soil   MEMS
#> 17:     7      MOM 1.149777e+01 mg C/g soil   MEMS
#> 18:     8      MOM 1.150942e+01 mg C/g soil   MEMS
#> 19:     9      MOM 1.151476e+01 mg C/g soil   MEMS
#> 20:    10      MOM 1.151705e+01 mg C/g soil   MEMS
#> 21:     1      QOM 1.000000e-01 mg C/g soil   MEMS
#> 22:     2      QOM 1.135088e-01 mg C/g soil   MEMS
#> 23:     3      QOM 1.171584e-01 mg C/g soil   MEMS
#> 24:     4      QOM 1.187808e-01 mg C/g soil   MEMS
#> 25:     5      QOM 1.195438e-01 mg C/g soil   MEMS
#> 26:     6      QOM 1.198953e-01 mg C/g soil   MEMS
#> 27:     7      QOM 1.200481e-01 mg C/g soil   MEMS
#> 28:     8      QOM 1.201049e-01 mg C/g soil   MEMS
#> 29:     9      QOM 1.201152e-01 mg C/g soil   MEMS
#> 30:    10      QOM 1.201032e-01 mg C/g soil   MEMS
#> 31:     1       MB 2.000000e+00 mg C/g soil   MEMS
#> 32:     2       MB 2.013714e+00 mg C/g soil   MEMS
#> 33:     3       MB 1.013229e+00 mg C/g soil   MEMS
#> 34:     4       MB 4.910671e-01 mg C/g soil   MEMS
#> 35:     5       MB 2.375647e-01 mg C/g soil   MEMS
#> 36:     6       MB 1.150177e-01 mg C/g soil   MEMS
#> 37:     7       MB 5.579123e-02 mg C/g soil   MEMS
#> 38:     8       MB 2.716780e-02 mg C/g soil   MEMS
#> 39:     9       MB 1.333500e-02 mg C/g soil   MEMS
#> 40:    10       MB 6.649165e-03 mg C/g soil   MEMS
#> 41:     1      DOM 1.000000e+00 mg C/g soil   MEMS
#> 42:     2      DOM 9.976932e-01 mg C/g soil   MEMS
#> 43:     3      DOM 4.055658e-01 mg C/g soil   MEMS
#> 44:     4      DOM 1.939209e-01 mg C/g soil   MEMS
#> 45:     5      DOM 9.385554e-02 mg C/g soil   MEMS
#> 46:     6      DOM 4.556182e-02 mg C/g soil   MEMS
#> 47:     7      DOM 2.222404e-02 mg C/g soil   MEMS
#> 48:     8      DOM 1.094514e-02 mg C/g soil   MEMS
#> 49:     9      DOM 5.493829e-03 mg C/g soil   MEMS
#> 50:    10      DOM 2.859810e-03 mg C/g soil   MEMS
#> 51:     1       EP 1.000000e-05 mg C/g soil   MEMS
#> 52:     2       EP 2.781807e-02 mg C/g soil   MEMS
#> 53:     3       EP 4.541118e-02 mg C/g soil   MEMS
#> 54:     4       EP 5.401600e-02 mg C/g soil   MEMS
#> 55:     5       EP 5.814889e-02 mg C/g soil   MEMS
#> 56:     6       EP 6.011679e-02 mg C/g soil   MEMS
#> 57:     7       EP 6.103831e-02 mg C/g soil   MEMS
#> 58:     8       EP 6.145415e-02 mg C/g soil   MEMS
#> 59:     9       EP 6.162562e-02 mg C/g soil   MEMS
#> 60:    10       EP 6.167904e-02 mg C/g soil   MEMS
#> 61:     1       EM 1.000000e-05 mg C/g soil   MEMS
#> 62:     2       EM 2.781807e-02 mg C/g soil   MEMS
#> 63:     3       EM 4.541118e-02 mg C/g soil   MEMS
#> 64:     4       EM 5.401600e-02 mg C/g soil   MEMS
#> 65:     5       EM 5.814889e-02 mg C/g soil   MEMS
#> 66:     6       EM 6.011679e-02 mg C/g soil   MEMS
#> 67:     7       EM 6.103831e-02 mg C/g soil   MEMS
#> 68:     8       EM 6.145415e-02 mg C/g soil   MEMS
#> 69:     9       EM 6.162562e-02 mg C/g soil   MEMS
#> 70:    10       EM 6.167904e-02 mg C/g soil   MEMS
#> 71:     1       IC 0.000000e+00 mg C/g soil   MEMS
#> 72:     2       IC 4.193876e+00 mg C/g soil   MEMS
#> 73:     3       IC 5.337757e+00 mg C/g soil   MEMS
#> 74:     4       IC 5.852773e+00 mg C/g soil   MEMS
#> 75:     5       IC 6.100903e+00 mg C/g soil   MEMS
#> 76:     6       IC 6.221155e+00 mg C/g soil   MEMS
#> 77:     7       IC 6.279638e+00 mg C/g soil   MEMS
#> 78:     8       IC 6.308271e+00 mg C/g soil   MEMS
#> 79:     9       IC 6.322476e+00 mg C/g soil   MEMS
#> 80:    10       IC 6.329709e+00 mg C/g soil   MEMS
#> 81:     1      Tot 1.810002e+01 mg C/g soil   MEMS
#> 82:     2      Tot 1.390614e+01 mg C/g soil   MEMS
#> 83:     3      Tot 1.276226e+01 mg C/g soil   MEMS
#> 84:     4      Tot 1.224725e+01 mg C/g soil   MEMS
#> 85:     5      Tot 1.199912e+01 mg C/g soil   MEMS
#> 86:     6      Tot 1.187887e+01 mg C/g soil   MEMS
#> 87:     7      Tot 1.182038e+01 mg C/g soil   MEMS
#> 88:     8      Tot 1.179175e+01 mg C/g soil   MEMS
#> 89:     9      Tot 1.177754e+01 mg C/g soil   MEMS
#> 90:    10      Tot 1.177031e+01 mg C/g soil   MEMS
#>      time variable        value       units   name
```
