# The MEMC model configuration for COMISSION

The COMISSION configuration is based off of Ahrens et al. 2015. The
model uses MM for the DOM uptake, RMM for POM decomposition, and LM for
MB decay; see
[dynamics](https://microbial-explicit-model.github.io/MEMC/reference/dynamics.md)
for more details.

## Usage

``` r
COMISSION_config
```

## Format

An object created from
[`memc_configure`](https://microbial-explicit-model.github.io/MEMC/reference/memc_configure.md),
containing the following elements.

- name:

  COMISSION

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

Ahrens et al.: Contribution of sorption, DOC transport and microbial
interactions to the 14C age of a soil organic carbon profile: Insights
from a calibrated process model, Soil Biol. Biochem., 88, 390–402, 2015.
<https://doi.org/10.1016/j.soilbio.2015.06.008>

## See also

Other configurations:
[`BAMS_config`](https://microbial-explicit-model.github.io/MEMC/reference/BAMS_config.md),
[`CORPSE_config`](https://microbial-explicit-model.github.io/MEMC/reference/CORPSE_config.md),
[`MEMS_config`](https://microbial-explicit-model.github.io/MEMC/reference/MEMS_config.md),
[`MEND_config`](https://microbial-explicit-model.github.io/MEMC/reference/MEND_config.md),
[`MIMCS_config`](https://microbial-explicit-model.github.io/MEMC/reference/MIMCS_config.md)

## Examples

``` r
## Use memc_solve to run the simulation using this configuration.
print(COMISSION_config)
#> $name
#> [1] "COMISSION"
#> 
#> $table
#>       model DOMuptake POMdecomp MBdecay
#> 1 COMISSION        MM       RMM      LM
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
memc_solve(mod = COMISSION_config, time = 1:10)
#>      time variable        value       units      name
#>     <num>   <fctr>        <num>      <char>    <char>
#>  1:     1      POM 10.000000000 mg C/g soil COMISSION
#>  2:     2      POM 10.850213363 mg C/g soil COMISSION
#>  3:     3      POM 11.136937769 mg C/g soil COMISSION
#>  4:     4      POM 11.192594531 mg C/g soil COMISSION
#>  5:     5      POM 11.159856461 mg C/g soil COMISSION
#>  6:     6      POM 11.092776214 mg C/g soil COMISSION
#>  7:     7      POM 11.012062434 mg C/g soil COMISSION
#>  8:     8      POM 10.925892509 mg C/g soil COMISSION
#>  9:     9      POM 10.837650895 mg C/g soil COMISSION
#> 10:    10      POM 10.748818233 mg C/g soil COMISSION
#> 11:     1      MOM  5.000000000 mg C/g soil COMISSION
#> 12:     2      MOM  5.015035658 mg C/g soil COMISSION
#> 13:     3      MOM  5.049106719 mg C/g soil COMISSION
#> 14:     4      MOM  5.090911104 mg C/g soil COMISSION
#> 15:     5      MOM  5.135605513 mg C/g soil COMISSION
#> 16:     6      MOM  5.181254864 mg C/g soil COMISSION
#> 17:     7      MOM  5.227081572 mg C/g soil COMISSION
#> 18:     8      MOM  5.272761941 mg C/g soil COMISSION
#> 19:     9      MOM  5.318154825 mg C/g soil COMISSION
#> 20:    10      MOM  5.363196054 mg C/g soil COMISSION
#> 21:     1      QOM  0.100000000 mg C/g soil COMISSION
#> 22:     2      QOM  0.101259600 mg C/g soil COMISSION
#> 23:     3      QOM  0.101621666 mg C/g soil COMISSION
#> 24:     4      QOM  0.102044784 mg C/g soil COMISSION
#> 25:     5      QOM  0.102572240 mg C/g soil COMISSION
#> 26:     6      QOM  0.103249616 mg C/g soil COMISSION
#> 27:     7      QOM  0.104114693 mg C/g soil COMISSION
#> 28:     8      QOM  0.105193330 mg C/g soil COMISSION
#> 29:     9      QOM  0.106500807 mg C/g soil COMISSION
#> 30:    10      QOM  0.108044934 mg C/g soil COMISSION
#> 31:     1       MB  2.000000000 mg C/g soil COMISSION
#> 32:     2       MB  0.935326286 mg C/g soil COMISSION
#> 33:     3       MB  0.363380965 mg C/g soil COMISSION
#> 34:     4       MB  0.145509605 mg C/g soil COMISSION
#> 35:     5       MB  0.061094144 mg C/g soil COMISSION
#> 36:     6       MB  0.027240745 mg C/g soil COMISSION
#> 37:     7       MB  0.012956400 mg C/g soil COMISSION
#> 38:     8       MB  0.006557010 mg C/g soil COMISSION
#> 39:     9       MB  0.003509625 mg C/g soil COMISSION
#> 40:    10       MB  0.001973103 mg C/g soil COMISSION
#> 41:     1      DOM  1.000000000 mg C/g soil COMISSION
#> 42:     2      DOM  0.064207702 mg C/g soil COMISSION
#> 43:     3      DOM  0.071469666 mg C/g soil COMISSION
#> 44:     4      DOM  0.085483326 mg C/g soil COMISSION
#> 45:     5      DOM  0.107471548 mg C/g soil COMISSION
#> 46:     6      DOM  0.136868435 mg C/g soil COMISSION
#> 47:     7      DOM  0.171785211 mg C/g soil COMISSION
#> 48:     8      DOM  0.210199534 mg C/g soil COMISSION
#> 49:     9      DOM  0.250564563 mg C/g soil COMISSION
#> 50:    10      DOM  0.291865178 mg C/g soil COMISSION
#> 51:     1       EP  0.000010000 mg C/g soil COMISSION
#> 52:     2       EP  0.017966874 mg C/g soil COMISSION
#> 53:     3       EP  0.025191363 mg C/g soil COMISSION
#> 54:     4       EP  0.028012139 mg C/g soil COMISSION
#> 55:     5       EP  0.029145477 mg C/g soil COMISSION
#> 56:     6       EP  0.029616385 mg C/g soil COMISSION
#> 57:     7       EP  0.029816099 mg C/g soil COMISSION
#> 58:     8       EP  0.029898438 mg C/g soil COMISSION
#> 59:     9       EP  0.029926775 mg C/g soil COMISSION
#> 60:    10       EP  0.029928740 mg C/g soil COMISSION
#> 61:     1       EM  0.000010000 mg C/g soil COMISSION
#> 62:     2       EM  0.017966874 mg C/g soil COMISSION
#> 63:     3       EM  0.025191363 mg C/g soil COMISSION
#> 64:     4       EM  0.028012139 mg C/g soil COMISSION
#> 65:     5       EM  0.029145477 mg C/g soil COMISSION
#> 66:     6       EM  0.029616385 mg C/g soil COMISSION
#> 67:     7       EM  0.029816099 mg C/g soil COMISSION
#> 68:     8       EM  0.029898438 mg C/g soil COMISSION
#> 69:     9       EM  0.029926775 mg C/g soil COMISSION
#> 70:    10       EM  0.029928740 mg C/g soil COMISSION
#> 71:     1       IC  0.000000000 mg C/g soil COMISSION
#> 72:     2       IC  1.098043644 mg C/g soil COMISSION
#> 73:     3       IC  1.327120490 mg C/g soil COMISSION
#> 74:     4       IC  1.427452371 mg C/g soil COMISSION
#> 75:     5       IC  1.475129139 mg C/g soil COMISSION
#> 76:     6       IC  1.499397357 mg C/g soil COMISSION
#> 77:     7       IC  1.512387492 mg C/g soil COMISSION
#> 78:     8       IC  1.519618800 mg C/g soil COMISSION
#> 79:     9       IC  1.523785734 mg C/g soil COMISSION
#> 80:    10       IC  1.526265018 mg C/g soil COMISSION
#> 81:     1      Tot 18.100020000 mg C/g soil COMISSION
#> 82:     2      Tot 17.001976356 mg C/g soil COMISSION
#> 83:     3      Tot 16.772899510 mg C/g soil COMISSION
#> 84:     4      Tot 16.672567629 mg C/g soil COMISSION
#> 85:     5      Tot 16.624890861 mg C/g soil COMISSION
#> 86:     6      Tot 16.600622643 mg C/g soil COMISSION
#> 87:     7      Tot 16.587632508 mg C/g soil COMISSION
#> 88:     8      Tot 16.580401200 mg C/g soil COMISSION
#> 89:     9      Tot 16.576234266 mg C/g soil COMISSION
#> 90:    10      Tot 16.573754982 mg C/g soil COMISSION
#>      time variable        value       units      name
```
