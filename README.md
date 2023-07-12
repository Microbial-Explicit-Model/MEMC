
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MEMC

[![R-CMD](https://github.com/Microbial-Explicit-Model/MEMC/actions/workflows/rcmd.yml/badge.svg)](https://github.com/Microbial-Explicit-Model/MEMC/actions/workflows/rcmd.yml)
[![codecov](https://codecov.io/gh/Microbial-Explicit-Model/MEMC/branch/main/graph/badge.svg?token=MA69VLXPYP)](https://codecov.io/gh/Microbial-Explicit-Model/MEMC)
[![Documentation](https://github.com/Microbial-Explicit-Model/MEMC/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/Microbial-Explicit-Model/MEMC/actions/workflows/pkgdown.yaml)

`MEMC` is a R package that allows users to explore various
representation of soil organic matter (SOM) flux dynamics within a
consistent SOM pool model structure. Popular microbial-explicit SOM
models such as MEND[^1], MIMICS[^2], and CORPSE[^3] vary significantly
in their formulations of microbial mechanisms and underlying pool
structure. `MEMC` provides a consistent pool structure with flexible
flux dynamics which allows modelers to easily explore the effects of the
different conceptualizations of microbial mechanisms.

## Installation

Follow the download and installation instructions for
[R](https://cloud.r-project.org/) and [R
studio](https://www.rstudio.com/products/rstudio/download/).

Use `remotes` to install MEMC as a built R package directory from
github.

    # use install.packages("remotes") to install this package the first time.
    library(remotes)

    # Now build and install the R package on your local machine.
    install_github('Microbial-Explicit-Model/MEMC') 

## Example

The package ships with several already defined model configurations, use
`help(configurations)` to see a list of all the available configurations
that are ready for use. Here we will demonstrate how to complete a
simulation using the MEND_model configuration. For more examples and
package details please checkout our [online
documentation](https://microbial-explicit-model.github.io/MEMC/).

``` r
# Load the installed MEMC package
library(MEMC)
```

Take a look at the pre-built MEND_model configuration (see
`help("MEND_model")` for more details).

``` r
# Printing this MEMC model configuration will return a list defining the run name, a table of the flux dynamics, parameter values, and initial SOM pool sizes. 
print(MEND_model)
#> $name
#> [1] "MEND"
#> 
#> $table
#>   model DOMdecomp POMdecomp MBdecay
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
#> 19 Input_MOM                                               MOM input 
#> 20       CUE                                    carbon use efficiency
#>              units   value
#> 1  mgC mgC^-1 h^-1  14.000
#> 2     mgC / g soil  50.000
#> 3  mgC mgC^-1 h^-1   0.250
#> 4      mg C/g soil 250.000
#> 5  mgC mgC^-1 h^-1   3.000
#> 6      mg C/g soil   0.250
#> 7             <NA>   0.500
#> 8             <NA>   0.500
#> 9             <NA>   0.010
#> 10            <NA>   0.010
#> 11 mgC mgC^-1 h^-1   0.001
#> 12 mgC mgC^-1 h^-1   0.001
#> 13    mgC / g soil   3.400
#> 14 mgC mgC^-1 h^-1   0.006
#> 15 mgC mgC^-1 h^-1   0.001
#> 16            <NA>   1.000
#> 17            mg C   0.000
#> 18            mg C   0.000
#> 19            mg C   0.000
#> 20                   0.400
#> 
#> $state
#>      POM      MOM      QOM       MB      DOM       EP       EM       IC 
#> 10.00000  5.00000  0.10000  2.00000  1.00000  0.00001  0.00001  0.00000 
#>      Tot 
#> 18.10002
```

Complete a model run using the `solve_model` function.

``` r
time <- seq(0, 36500, by=25) 
mend_out <- solve_model(mod = MEND_model, time = time)
```

The model run results are saved in the `mend_out` data frame.

``` r
# Preview the run results
head(mend_out)
#>    time variable     value       units name
#> 1:    0      POM 10.000000 mg C/g soil MEND
#> 2:   25      POM  9.742068 mg C/g soil MEND
#> 3:   50      POM  8.200540 mg C/g soil MEND
#> 4:   75      POM  6.903752 mg C/g soil MEND
#> 5:  100      POM  5.816141 mg C/g soil MEND
#> 6:  125      POM  4.905867 mg C/g soil MEND
```

Visualize the run results.

``` r
ggplot(data = mend_out) + 
  geom_line(aes(time, value, color = name), linewidth = 0.75) + 
  facet_wrap("variable", scales = "free") + 
  labs(y = "mg C/g soil", 
       title = "MEND Run Results")
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

With `MEMC` users are able easily run simulations with the provide model
configurations and are able to build toy model of their own design by
selecting any combination of the supported flux dynamics. See here for
an example for how to use `configure_model` to build your own SOM model.
For this example we will use the default parameter and initial pool
values that are included as package data (see `help("default_params)`
and `help("default_initial)` for more information).

``` r
# Running configure_model will print a table describing the model configuration. 
my_model <- configure_model(params = default_params, 
                            state = default_initial, 
                            name = "my model", 
                            DOMdecomp = "MM", 
                            POMdecomp = "LM", 
                            MBdecay = "LM")
#> |model    |DOMdecomp |POMdecomp |MBdecay |
#> |:--------|:---------|:---------|:-------|
#> |my model |MM        |LM        |LM      |
```

Use `solve_model` to run our model.

``` r
time <- seq(0, 36500, by=25) 
my_out <- solve_model(mod = my_model, time = time)
```

Compare our toy model results with the MEND model results.

<img src="man/figures/README-unnamed-chunk-9-1.png" width="100%" />

By changing the POM and DOM decomposition flux dynamics affects model
behavior! The flexibility of the flux dynamics makes `MEMC` a powerful
tool for SOM model exploration. Additional features supported by `MEMC`
include the ability to change model parameters, perform sensitivity
analyses, and fit models with experimental/observational data (see
[online documentation](https://microbial-explicit-model.github.io/MEMC/)
for examples of these capabilities).

[^1]: Wang, Gangsheng, Sindhu Jagadamma, Melanie A. Mayes, Christopher
    W. Schadt, J. Megan Steinweg, Lianhong Gu, and Wilfred M. Post.
    2015. “Microbial Dormancy Improves Development and Experimental
    Validation of Ecosystem Model.” The ISME Journal 9 (1): 226–37.

[^2]: .Wieder, William R., Steven D. Allison, Eric A. Davidson, Katerina
    Georgiou, Oleksandra Hararuk, Yujie He, Francesca Hopkins, et
    al. 2015. “Explicitly Representing Soil Microbial Processes in Earth
    System Models.” Global Biogeochemical Cycles 29 (10): 1782–1800.

[^3]: Sulman, Benjamin N., Jessica A. M. Moore, Rose Abramoff, Colin
    Averill, Stephanie Kivlin, Katerina Georgiou, Bhavya Sridhar, et
    al. 2018. “Multiple Models and Experiments Underscore Large
    Uncertainty in Soil Carbon Dynamics.” Biogeochemistry 141 (2):
    109–23.
