# The default initial carbon pool values used by the default MEMC model configurations

The default initial carbon pool values used by the default MEMC model
configurations

## Usage

``` r
memc_initial_state
```

## Format

A named vector of the initial states of the carbon pools.

- POM:

  Particulate organic carbon or POC

- MOM:

  Mineral associated organic carbon or MOC, note that this excludes QOM

- QOM:

  Adsorbed phase of DOC

- MB:

  Microbial biomass carbon or MBC

- DOM:

  Dissolved organic carbon or DOC

- EP:

  Enzyme for decomposition of POM

- EM:

  Enzyme for decomposition of MOM

- IC:

  Accumulated CO2 flux

- Tot:

  Total carbon in the patch

## References

Wang et al.: Development of microbial-enzyme-mediated decomposition
model parameters through steady-state and dynamic analyses, Ecol. Appl.,
23, 255–272, 2013. <https://doi.org/10.1890/12-0681.1>

## See also

Other inputs:
[`memc_params`](https://microbial-explicit-model.github.io/MEMC/reference/memc_params.md)

## Examples

``` r
print(memc_initial_state)
#>      POM      MOM      QOM       MB      DOM       EP       EM       IC 
#> 10.00000  5.00000  0.10000  2.00000  1.00000  0.00001  0.00001  0.00000 
#>      Tot 
#> 18.10002 
```
