# The MEMC model configuration for MEND

The MEND configuration is based off of Wang et al. 2013. This
configuration uses MM for DOM uptake, MM for POM decomposition, and LM
for MB decay; see
[dynamics](https://microbial-explicit-model.github.io/MEMC/reference/dynamics.md)
for more details.

## Usage

``` r
MEND_config
```

## Format

An object created from
[`memc_configure`](https://microbial-explicit-model.github.io/MEMC/reference/memc_configure.md).

- name:

  MEND

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

Wang et al.: Development of microbial-enzyme-mediated decomposition
model parameters through steady-state and dynamic analyses, Ecol. Appl.,
23, 255–272, 2013. <https://doi.org/10.1890/12-0681.1>

## See also

Other configurations:
[`BAMS_config`](https://microbial-explicit-model.github.io/MEMC/reference/BAMS_config.md),
[`COMISSION_config`](https://microbial-explicit-model.github.io/MEMC/reference/COMISSION_config.md),
[`CORPSE_config`](https://microbial-explicit-model.github.io/MEMC/reference/CORPSE_config.md),
[`MEMS_config`](https://microbial-explicit-model.github.io/MEMC/reference/MEMS_config.md),
[`MIMCS_config`](https://microbial-explicit-model.github.io/MEMC/reference/MIMCS_config.md)

## Examples

``` r
## Use memc_solve to run the simulation using this configuration.
print(MEND_config)
#> $name
#> [1] "MEND"
#> 
#> $table
#>   model DOMuptake POMdecomp MBdecay
#> 1  MEND        MM        MM      LM
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
memc_solve(mod = MEND_config, time = 1:10)
#>      time variable        value       units   name
#>     <num>   <fctr>        <num>      <char> <char>
#>  1:     1      POM 10.000000000 mg C/g soil   MEND
#>  2:     2      POM 10.855361397 mg C/g soil   MEND
#>  3:     3      POM 11.153753353 mg C/g soil   MEND
#>  4:     4      POM 11.223634295 mg C/g soil   MEND
#>  5:     5      POM 11.206083983 mg C/g soil   MEND
#>  6:     6      POM 11.154576883 mg C/g soil   MEND
#>  7:     7      POM 11.089568492 mg C/g soil   MEND
#>  8:     8      POM 11.019084570 mg C/g soil   MEND
#>  9:     9      POM 10.946405245 mg C/g soil   MEND
#> 10:    10      POM 10.872939845 mg C/g soil   MEND
#> 11:     1      MOM  5.000000000 mg C/g soil   MEND
#> 12:     2      MOM  5.012398354 mg C/g soil   MEND
#> 13:     3      MOM  5.040316423 mg C/g soil   MEND
#> 14:     4      MOM  5.074505205 mg C/g soil   MEND
#> 15:     5      MOM  5.111070420 mg C/g soil   MEND
#> 16:     6      MOM  5.148467662 mg C/g soil   MEND
#> 17:     7      MOM  5.186080676 mg C/g soil   MEND
#> 18:     8      MOM  5.223655417 mg C/g soil   MEND
#> 19:     9      MOM  5.261082319 mg C/g soil   MEND
#> 20:    10      MOM  5.298311884 mg C/g soil   MEND
#> 21:     1      QOM  0.100000000 mg C/g soil   MEND
#> 22:     2      QOM  0.101258165 mg C/g soil   MEND
#> 23:     3      QOM  0.101613518 mg C/g soil   MEND
#> 24:     4      QOM  0.102018644 mg C/g soil   MEND
#> 25:     5      QOM  0.102509140 mg C/g soil   MEND
#> 26:     6      QOM  0.103122851 mg C/g soil   MEND
#> 27:     7      QOM  0.103891499 mg C/g soil   MEND
#> 28:     8      QOM  0.104837020 mg C/g soil   MEND
#> 29:     9      QOM  0.105972524 mg C/g soil   MEND
#> 30:    10      QOM  0.107304859 mg C/g soil   MEND
#> 31:     1       MB  2.000000000 mg C/g soil   MEND
#> 32:     2       MB  0.934701108 mg C/g soil   MEND
#> 33:     3       MB  0.361892620 mg C/g soil   MEND
#> 34:     4       MB  0.143662614 mg C/g soil   MEND
#> 35:     5       MB  0.059352006 mg C/g soil   MEND
#> 36:     6       MB  0.025826064 mg C/g soil   MEND
#> 37:     7       MB  0.011900144 mg C/g soil   MEND
#> 38:     8       MB  0.005802372 mg C/g soil   MEND
#> 39:     9       MB  0.002981106 mg C/g soil   MEND
#> 40:    10       MB  0.001604872 mg C/g soil   MEND
#> 41:     1      DOM  1.000000000 mg C/g soil   MEND
#> 42:     2      DOM  0.063654080 mg C/g soil   MEND
#> 43:     3      DOM  0.069547559 mg C/g soil   MEND
#> 44:     4      DOM  0.080993830 mg C/g soil   MEND
#> 45:     5      DOM  0.099024321 mg C/g soil   MEND
#> 46:     6      DOM  0.123240408 mg C/g soil   MEND
#> 47:     7      DOM  0.152114563 mg C/g soil   MEND
#> 48:     8      DOM  0.183967832 mg C/g soil   MEND
#> 49:     9      DOM  0.217502598 mg C/g soil   MEND
#> 50:    10      DOM  0.251867257 mg C/g soil   MEND
#> 51:     1       EP  0.000010000 mg C/g soil   MEND
#> 52:     2       EP  0.017964291 mg C/g soil   MEND
#> 53:     3       EP  0.025175752 mg C/g soil   MEND
#> 54:     4       EP  0.027975984 mg C/g soil   MEND
#> 55:     5       EP  0.029087487 mg C/g soil   MEND
#> 56:     6       EP  0.029539414 mg C/g soil   MEND
#> 57:     7       EP  0.029724417 mg C/g soil   MEND
#> 58:     8       EP  0.029796061 mg C/g soil   MEND
#> 59:     9       EP  0.029816877 mg C/g soil   MEND
#> 60:    10       EP  0.029813633 mg C/g soil   MEND
#> 61:     1       EM  0.000010000 mg C/g soil   MEND
#> 62:     2       EM  0.017964291 mg C/g soil   MEND
#> 63:     3       EM  0.025175752 mg C/g soil   MEND
#> 64:     4       EM  0.027975984 mg C/g soil   MEND
#> 65:     5       EM  0.029087487 mg C/g soil   MEND
#> 66:     6       EM  0.029539414 mg C/g soil   MEND
#> 67:     7       EM  0.029724417 mg C/g soil   MEND
#> 68:     8       EM  0.029796061 mg C/g soil   MEND
#> 69:     9       EM  0.029816877 mg C/g soil   MEND
#> 70:    10       EM  0.029813633 mg C/g soil   MEND
#> 71:     1       IC  0.000000000 mg C/g soil   MEND
#> 72:     2       IC  1.096718313 mg C/g soil   MEND
#> 73:     3       IC  1.322545023 mg C/g soil   MEND
#> 74:     4       IC  1.419253443 mg C/g soil   MEND
#> 75:     5       IC  1.463805157 mg C/g soil   MEND
#> 76:     6       IC  1.485707304 mg C/g soil   MEND
#> 77:     7       IC  1.497015792 mg C/g soil   MEND
#> 78:     8       IC  1.503080668 mg C/g soil   MEND
#> 79:     9       IC  1.506442454 mg C/g soil   MEND
#> 80:    10       IC  1.508364017 mg C/g soil   MEND
#> 81:     1      Tot 18.100020000 mg C/g soil   MEND
#> 82:     2      Tot 17.003301687 mg C/g soil   MEND
#> 83:     3      Tot 16.777474977 mg C/g soil   MEND
#> 84:     4      Tot 16.680766557 mg C/g soil   MEND
#> 85:     5      Tot 16.636214843 mg C/g soil   MEND
#> 86:     6      Tot 16.614312696 mg C/g soil   MEND
#> 87:     7      Tot 16.603004208 mg C/g soil   MEND
#> 88:     8      Tot 16.596939332 mg C/g soil   MEND
#> 89:     9      Tot 16.593577546 mg C/g soil   MEND
#> 90:    10      Tot 16.591655983 mg C/g soil   MEND
#>      time variable        value       units   name
```
