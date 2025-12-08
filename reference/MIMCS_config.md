# The MEMC model configuration for MIMCS

The MIMCS configuration is based off of Wieder et al. 2015. This model
uses MM for the DOM uptake, MM for POM decomposition, and DD for MB
decay; see
[dynamics](https://microbial-explicit-model.github.io/MEMC/reference/dynamics.md)
for more details.

## Usage

``` r
MIMCS_config
```

## Format

An object created from
[`memc_configure`](https://microbial-explicit-model.github.io/MEMC/reference/memc_configure.md),
containing the following elements.

- name:

  MIMCS

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

Wieder, W. R., et al.: Explicitly representing soil microbial processes
in Earth system models, Global Biogeochem. Cycles, 29, 1782–1800, 2015.
<https://doi.org/10.1002/2015GB005188>

## See also

Other configurations:
[`BAMS_config`](https://microbial-explicit-model.github.io/MEMC/reference/BAMS_config.md),
[`COMISSION_config`](https://microbial-explicit-model.github.io/MEMC/reference/COMISSION_config.md),
[`CORPSE_config`](https://microbial-explicit-model.github.io/MEMC/reference/CORPSE_config.md),
[`MEMS_config`](https://microbial-explicit-model.github.io/MEMC/reference/MEMS_config.md),
[`MEND_config`](https://microbial-explicit-model.github.io/MEMC/reference/MEND_config.md)

## Examples

``` r
## Use memc_solve to run the simulation using this configuration.
print(MIMCS_config)
#> $name
#> [1] "MIMCS"
#> 
#> $table
#>   model DOMuptake POMdecomp MBdecay
#> 1 MIMCS        MM        MM      DD
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
#> 16            <NA>   2.000
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
memc_solve(mod = MIMCS_config, time = 1:10)
#>      time variable       value       units   name
#>     <num>   <fctr>       <num>      <char> <char>
#>  1:     1      POM 10.00000000 mg C/g soil  MIMCS
#>  2:     2      POM 10.95619098 mg C/g soil  MIMCS
#>  3:     3      POM 11.11889197 mg C/g soil  MIMCS
#>  4:     4      POM 11.14272636 mg C/g soil  MIMCS
#>  5:     5      POM 11.11848609 mg C/g soil  MIMCS
#>  6:     6      POM 11.07038243 mg C/g soil  MIMCS
#>  7:     7      POM 11.00786996 mg C/g soil  MIMCS
#>  8:     8      POM 10.93551315 mg C/g soil  MIMCS
#>  9:     9      POM 10.85583358 mg C/g soil  MIMCS
#> 10:    10      POM 10.77036028 mg C/g soil  MIMCS
#> 11:     1      MOM  5.00000000 mg C/g soil  MIMCS
#> 12:     2      MOM  5.01063569 mg C/g soil  MIMCS
#> 13:     3      MOM  5.03438974 mg C/g soil  MIMCS
#> 14:     4      MOM  5.06540822 mg C/g soil  MIMCS
#> 15:     5      MOM  5.10137631 mg C/g soil  MIMCS
#> 16:     6      MOM  5.14109633 mg C/g soil  MIMCS
#> 17:     7      MOM  5.18385016 mg C/g soil  MIMCS
#> 18:     8      MOM  5.22917041 mg C/g soil  MIMCS
#> 19:     9      MOM  5.27673710 mg C/g soil  MIMCS
#> 20:    10      MOM  5.32632365 mg C/g soil  MIMCS
#> 21:     1      QOM  0.10000000 mg C/g soil  MIMCS
#> 22:     2      QOM  0.10178079 mg C/g soil  MIMCS
#> 23:     3      QOM  0.10199580 mg C/g soil  MIMCS
#> 24:     4      QOM  0.10213966 mg C/g soil  MIMCS
#> 25:     5      QOM  0.10226937 mg C/g soil  MIMCS
#> 26:     6      QOM  0.10240270 mg C/g soil  MIMCS
#> 27:     7      QOM  0.10254738 mg C/g soil  MIMCS
#> 28:     8      QOM  0.10270683 mg C/g soil  MIMCS
#> 29:     9      QOM  0.10288220 mg C/g soil  MIMCS
#> 30:    10      QOM  0.10307326 mg C/g soil  MIMCS
#> 31:     1       MB  2.00000000 mg C/g soil  MIMCS
#> 32:     2       MB  0.78594185 mg C/g soil  MIMCS
#> 33:     3       MB  0.45507469 mg C/g soil  MIMCS
#> 34:     4       MB  0.32269533 mg C/g soil  MIMCS
#> 35:     5       MB  0.25374428 mg C/g soil  MIMCS
#> 36:     6       MB  0.21294150 mg C/g soil  MIMCS
#> 37:     7       MB  0.18709516 mg C/g soil  MIMCS
#> 38:     8       MB  0.17014106 mg C/g soil  MIMCS
#> 39:     9       MB  0.15888931 mg C/g soil  MIMCS
#> 40:    10       MB  0.15148798 mg C/g soil  MIMCS
#> 41:     1      DOM  1.00000000 mg C/g soil  MIMCS
#> 42:     2      DOM  0.05919225 mg C/g soil  MIMCS
#> 43:     3      DOM  0.03309850 mg C/g soil  MIMCS
#> 44:     4      DOM  0.02791116 mg C/g soil  MIMCS
#> 45:     5      DOM  0.02745580 mg C/g soil  MIMCS
#> 46:     6      DOM  0.02891942 mg C/g soil  MIMCS
#> 47:     7      DOM  0.03125726 mg C/g soil  MIMCS
#> 48:     8      DOM  0.03395304 mg C/g soil  MIMCS
#> 49:     9      DOM  0.03670977 mg C/g soil  MIMCS
#> 50:    10      DOM  0.03934787 mg C/g soil  MIMCS
#> 51:     1       EP  0.00001000 mg C/g soil  MIMCS
#> 52:     2       EP  0.01494581 mg C/g soil  MIMCS
#> 53:     3       EP  0.02201630 mg C/g soil  MIMCS
#> 54:     4       EP  0.02656158 mg C/g soil  MIMCS
#> 55:     5       EP  0.02995278 mg C/g soil  MIMCS
#> 56:     6       EP  0.03270196 mg C/g soil  MIMCS
#> 57:     7       EP  0.03505710 mg C/g soil  MIMCS
#> 58:     8       EP  0.03715743 mg C/g soil  MIMCS
#> 59:     9       EP  0.03908886 mg C/g soil  MIMCS
#> 60:    10       EP  0.04090793 mg C/g soil  MIMCS
#> 61:     1       EM  0.00001000 mg C/g soil  MIMCS
#> 62:     2       EM  0.01494581 mg C/g soil  MIMCS
#> 63:     3       EM  0.02201630 mg C/g soil  MIMCS
#> 64:     4       EM  0.02656158 mg C/g soil  MIMCS
#> 65:     5       EM  0.02995278 mg C/g soil  MIMCS
#> 66:     6       EM  0.03270196 mg C/g soil  MIMCS
#> 67:     7       EM  0.03505710 mg C/g soil  MIMCS
#> 68:     8       EM  0.03715743 mg C/g soil  MIMCS
#> 69:     9       EM  0.03908886 mg C/g soil  MIMCS
#> 70:    10       EM  0.04090793 mg C/g soil  MIMCS
#> 71:     1       IC  0.00000000 mg C/g soil  MIMCS
#> 72:     2       IC  1.15638683 mg C/g soil  MIMCS
#> 73:     3       IC  1.31253671 mg C/g soil  MIMCS
#> 74:     4       IC  1.38601611 mg C/g soil  MIMCS
#> 75:     5       IC  1.43678258 mg C/g soil  MIMCS
#> 76:     6       IC  1.47887370 mg C/g soil  MIMCS
#> 77:     7       IC  1.51728588 mg C/g soil  MIMCS
#> 78:     8       IC  1.55422064 mg C/g soil  MIMCS
#> 79:     9       IC  1.59079031 mg C/g soil  MIMCS
#> 80:    10       IC  1.62761110 mg C/g soil  MIMCS
#> 81:     1      Tot 18.10002000 mg C/g soil  MIMCS
#> 82:     2      Tot 16.94363317 mg C/g soil  MIMCS
#> 83:     3      Tot 16.78748329 mg C/g soil  MIMCS
#> 84:     4      Tot 16.71400389 mg C/g soil  MIMCS
#> 85:     5      Tot 16.66323742 mg C/g soil  MIMCS
#> 86:     6      Tot 16.62114630 mg C/g soil  MIMCS
#> 87:     7      Tot 16.58273412 mg C/g soil  MIMCS
#> 88:     8      Tot 16.54579936 mg C/g soil  MIMCS
#> 89:     9      Tot 16.50922969 mg C/g soil  MIMCS
#> 90:    10      Tot 16.47240890 mg C/g soil  MIMCS
#>      time variable       value       units   name
```
