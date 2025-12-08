# List of all the pre-defined MEMC models

List of all the pre-defined MEMC models

## Usage

``` r
memc_all_configs
```

## Format

A easy-to-iterate-over list, each element of which is one of the MEMC
models.

## See also

[`MEND_config`](https://microbial-explicit-model.github.io/MEMC/reference/MEND_config.md),
[`COMISSION_config`](https://microbial-explicit-model.github.io/MEMC/reference/COMISSION_config.md),
[`CORPSE_config`](https://microbial-explicit-model.github.io/MEMC/reference/CORPSE_config.md),
[`MEMS_config`](https://microbial-explicit-model.github.io/MEMC/reference/MEMS_config.md),
[`BAMS_config`](https://microbial-explicit-model.github.io/MEMC/reference/BAMS_config.md),
[`MIMCS_config`](https://microbial-explicit-model.github.io/MEMC/reference/MIMCS_config.md)

## Examples

``` r
memc_all_configs
#> $MEND
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
#> 
#> $COMISSION
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
#> 
#> $CORPSE
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
#> 
#> $MEMS
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
#> 
#> $BAMS
#> $name
#> [1] "BAMS"
#> 
#> $table
#>   model DOMuptake POMdecomp MBdecay
#> 1  BAMS        MM        MM      LM
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
#> 
#> $MIMICS
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
#> 

# Return the summary table for dynamics used in model configurations
summary(memc_all_configs)
#> 
#> 
#> |model     |DOMuptake |POMdecomp |MBdecay |
#> |:---------|:---------|:---------|:-------|
#> |MEND      |MM        |MM        |LM      |
#> |COMISSION |MM        |RMM       |LM      |
#> |CORPSE    |RMM       |LM        |LM      |
#> |MEMS      |LM        |LM        |LM      |
#> |BAMS      |MM        |MM        |LM      |
#> |MIMCS     |MM        |MM        |DD      |
```
