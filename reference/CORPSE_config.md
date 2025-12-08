# The MEMC model configuration for CORPSE

The CORPSE configuration is based off of Sulman et al. 2018. This model
uses RMM for DOM uptake, LM for POM decomposition, and LM for MB decay;
see
[dynamics](https://microbial-explicit-model.github.io/MEMC/reference/dynamics.md)
for more details.

## Usage

``` r
CORPSE_config
```

## Format

An object created from
[`memc_configure`](https://microbial-explicit-model.github.io/MEMC/reference/memc_configure.md),
containing the following elements.

- name:

  CORPSE

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

Sulman et al.: Multiple models and experiments underscore large
uncertainty in soil carbon dynamics, Biogeochemistry, 141, 109–123,
2018. <https://doi.org/10.1007/s10533-018-0509-z>

## See also

Other configurations:
[`BAMS_config`](https://microbial-explicit-model.github.io/MEMC/reference/BAMS_config.md),
[`COMISSION_config`](https://microbial-explicit-model.github.io/MEMC/reference/COMISSION_config.md),
[`MEMS_config`](https://microbial-explicit-model.github.io/MEMC/reference/MEMS_config.md),
[`MEND_config`](https://microbial-explicit-model.github.io/MEMC/reference/MEND_config.md),
[`MIMCS_config`](https://microbial-explicit-model.github.io/MEMC/reference/MIMCS_config.md)

## Examples

``` r
## Use memc_solve to run the simulation using this configuration.
print(CORPSE_config)
#> $name
#> [1] "CORPSE"
#> 
#> $table
#>    model DOMuptake POMdecomp MBdecay
#> 1 CORPSE       RMM        LM      LM
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
memc_solve(mod = CORPSE_config, time = 1:10)
#>      time variable        value       units   name
#>     <num>   <fctr>        <num>      <char> <char>
#>  1:     1      POM 1.000000e+01 mg C/g soil CORPSE
#>  2:     2      POM 8.702262e-02 mg C/g soil CORPSE
#>  3:     3      POM 4.495537e-02 mg C/g soil CORPSE
#>  4:     4      POM 2.186990e-02 mg C/g soil CORPSE
#>  5:     5      POM 1.058152e-02 mg C/g soil CORPSE
#>  6:     6      POM 5.117130e-03 mg C/g soil CORPSE
#>  7:     7      POM 2.474830e-03 mg C/g soil CORPSE
#>  8:     8      POM 1.197683e-03 mg C/g soil CORPSE
#>  9:     9      POM 5.800361e-04 mg C/g soil CORPSE
#> 10:    10      POM 2.810992e-04 mg C/g soil CORPSE
#> 11:     1      MOM 5.000000e+00 mg C/g soil CORPSE
#> 12:     2      MOM 1.062332e+01 mg C/g soil CORPSE
#> 13:     3      MOM 1.107449e+01 mg C/g soil CORPSE
#> 14:     4      MOM 1.129847e+01 mg C/g soil CORPSE
#> 15:     5      MOM 1.140671e+01 mg C/g soil CORPSE
#> 16:     6      MOM 1.145872e+01 mg C/g soil CORPSE
#> 17:     7      MOM 1.148352e+01 mg C/g soil CORPSE
#> 18:     8      MOM 1.149517e+01 mg C/g soil CORPSE
#> 19:     9      MOM 1.150047e+01 mg C/g soil CORPSE
#> 20:    10      MOM 1.150268e+01 mg C/g soil CORPSE
#> 21:     1      QOM 1.000000e-01 mg C/g soil CORPSE
#> 22:     2      QOM 1.145557e-01 mg C/g soil CORPSE
#> 23:     3      QOM 1.189325e-01 mg C/g soil CORPSE
#> 24:     4      QOM 1.211477e-01 mg C/g soil CORPSE
#> 25:     5      QOM 1.224851e-01 mg C/g soil CORPSE
#> 26:     6      QOM 1.234069e-01 mg C/g soil CORPSE
#> 27:     7      QOM 1.241285e-01 mg C/g soil CORPSE
#> 28:     8      QOM 1.247537e-01 mg C/g soil CORPSE
#> 29:     9      QOM 1.253326e-01 mg C/g soil CORPSE
#> 30:    10      QOM 1.258897e-01 mg C/g soil CORPSE
#> 31:     1       MB 2.000000e+00 mg C/g soil CORPSE
#> 32:     2       MB 1.991354e+00 mg C/g soil CORPSE
#> 33:     3       MB 1.015993e+00 mg C/g soil CORPSE
#> 34:     4       MB 4.937394e-01 mg C/g soil CORPSE
#> 35:     5       MB 2.388665e-01 mg C/g soil CORPSE
#> 36:     6       MB 1.155142e-01 mg C/g soil CORPSE
#> 37:     7       MB 5.587535e-02 mg C/g soil CORPSE
#> 38:     8       MB 2.704091e-02 mg C/g soil CORPSE
#> 39:     9       MB 1.309532e-02 mg C/g soil CORPSE
#> 40:    10       MB 6.346966e-03 mg C/g soil CORPSE
#> 41:     1      DOM 1.000000e+00 mg C/g soil CORPSE
#> 42:     2      DOM 1.158808e+00 mg C/g soil CORPSE
#> 43:     3      DOM 5.128399e-01 mg C/g soil CORPSE
#> 44:     4      DOM 2.942097e-01 mg C/g soil CORPSE
#> 45:     5      DOM 1.929027e-01 mg C/g soil CORPSE
#> 46:     6      DOM 1.442428e-01 mg C/g soil CORPSE
#> 47:     7      DOM 1.208025e-01 mg C/g soil CORPSE
#> 48:     8      DOM 1.095501e-01 mg C/g soil CORPSE
#> 49:     9      DOM 1.042048e-01 mg C/g soil CORPSE
#> 50:    10      DOM 1.017287e-01 mg C/g soil CORPSE
#> 51:     1       EP 1.000000e-05 mg C/g soil CORPSE
#> 52:     2       EP 2.721961e-02 mg C/g soil CORPSE
#> 53:     3       EP 4.475479e-02 mg C/g soil CORPSE
#> 54:     4       EP 5.339768e-02 mg C/g soil CORPSE
#> 55:     5       EP 5.755449e-02 mg C/g soil CORPSE
#> 56:     6       EP 5.953327e-02 mg C/g soil CORPSE
#> 57:     7       EP 6.045858e-02 mg C/g soil CORPSE
#> 58:     8       EP 6.087460e-02 mg C/g soil CORPSE
#> 59:     9       EP 6.104440e-02 mg C/g soil CORPSE
#> 60:    10       EP 6.109512e-02 mg C/g soil CORPSE
#> 61:     1       EM 1.000000e-05 mg C/g soil CORPSE
#> 62:     2       EM 2.721961e-02 mg C/g soil CORPSE
#> 63:     3       EM 4.475479e-02 mg C/g soil CORPSE
#> 64:     4       EM 5.339768e-02 mg C/g soil CORPSE
#> 65:     5       EM 5.755449e-02 mg C/g soil CORPSE
#> 66:     6       EM 5.953327e-02 mg C/g soil CORPSE
#> 67:     7       EM 6.045858e-02 mg C/g soil CORPSE
#> 68:     8       EM 6.087460e-02 mg C/g soil CORPSE
#> 69:     9       EM 6.104440e-02 mg C/g soil CORPSE
#> 70:    10       EM 6.109512e-02 mg C/g soil CORPSE
#> 71:     1       IC 0.000000e+00 mg C/g soil CORPSE
#> 72:     2       IC 4.070518e+00 mg C/g soil CORPSE
#> 73:     3       IC 5.243301e+00 mg C/g soil CORPSE
#> 74:     4       IC 5.763793e+00 mg C/g soil CORPSE
#> 75:     5       IC 6.013364e+00 mg C/g soil CORPSE
#> 76:     6       IC 6.133953e+00 mg C/g soil CORPSE
#> 77:     7       IC 6.192299e+00 mg C/g soil CORPSE
#> 78:     8       IC 6.220554e+00 mg C/g soil CORPSE
#> 79:     9       IC 6.234252e+00 mg C/g soil CORPSE
#> 80:    10       IC 6.240900e+00 mg C/g soil CORPSE
#> 81:     1      Tot 1.810002e+01 mg C/g soil CORPSE
#> 82:     2      Tot 1.402950e+01 mg C/g soil CORPSE
#> 83:     3      Tot 1.285672e+01 mg C/g soil CORPSE
#> 84:     4      Tot 1.233623e+01 mg C/g soil CORPSE
#> 85:     5      Tot 1.208666e+01 mg C/g soil CORPSE
#> 86:     6      Tot 1.196607e+01 mg C/g soil CORPSE
#> 87:     7      Tot 1.190772e+01 mg C/g soil CORPSE
#> 88:     8      Tot 1.187947e+01 mg C/g soil CORPSE
#> 89:     9      Tot 1.186577e+01 mg C/g soil CORPSE
#> 90:    10      Tot 1.185912e+01 mg C/g soil CORPSE
#>      time variable        value       units   name
```
