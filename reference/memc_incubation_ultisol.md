# Soil incubation respiration fluxes from Wang et al. 2013

A list containing incubation data and initial states.

## Usage

``` r
data(memc_incubation_ultisol)

memc_incubation_ultisol
```

## Format

A list containing:

- data:

  A data frame with 45 rows (15 time points x three replicates) and 3
  columns:

  Day

  :   Day of incubation.

  Soil

  :   String constant describing soil type.

  value

  :   Cumulative respiration, mg C/g soil.

- state:

  A named vector of the initial states of the carbon pools associated
  with the observations

A list of 2 elements:

- `data`:

  data frame of time-series data for carbon pools from the incubation
  experiment. The data frame contains the following variables:

  `Day`

  :   numeric, indicating the time since the start of the incubation
      experiment.

  `Soil`

  :   Character indicating the soil type (e.g., "Ultisol").

  `IC`

  :   Numeric value for inorganic carbon (mg C g\\^{-1}\\ soil).

- `state`:

  named numeric vector of initial state values for the soil carbon
  pools.

## Details

The \`data\` element contains time-series observations, while the
\`state\` element represents the initial conditions for the soil carbon
pools.

## References

Wang et al.: Development of microbial-enzyme-mediated decomposition
model parameters through steady-state and dynamic analyses, Ecol. Appl.,
23, 255–272, 2013. <https://doi.org/10.1890/12-0681.1>

## Examples

``` r
memc_incubation_ultisol
#> $data
#>     Day    Soil   IC
#> 181   1 Ultisol 0.12
#> 182   2 Ultisol 0.23
#> 183   4 Ultisol 0.37
#> 184   7 Ultisol 0.49
#> 185  10 Ultisol 0.77
#> 186  15 Ultisol 0.96
#> 187  31 Ultisol 1.65
#> 188  45 Ultisol 2.13
#> 189  60 Ultisol 2.46
#> 190  90 Ultisol 2.76
#> 191 120 Ultisol 3.07
#> 192 150 Ultisol 3.13
#> 193 210 Ultisol 3.48
#> 194 270 Ultisol 3.76
#> 195 365 Ultisol 3.99
#> 196   1 Ultisol 0.27
#> 197   2 Ultisol 0.40
#> 198   4 Ultisol 0.60
#> 199   7 Ultisol 0.71
#> 200  10 Ultisol 0.92
#> 201  15 Ultisol 1.01
#> 202  31 Ultisol 1.53
#> 203  45 Ultisol 1.80
#> 204  60 Ultisol 2.04
#> 205  90 Ultisol 2.23
#> 206 120 Ultisol 2.55
#> 207 150 Ultisol 2.64
#> 208 210 Ultisol 2.98
#> 209 270 Ultisol 3.30
#> 210 365 Ultisol 3.84
#> 211   1 Ultisol 0.20
#> 212   2 Ultisol 0.31
#> 213   4 Ultisol 0.50
#> 214   7 Ultisol 0.61
#> 215  10 Ultisol 0.83
#> 216  15 Ultisol 1.02
#> 217  31 Ultisol 1.81
#> 218  45 Ultisol 2.12
#> 219  60 Ultisol 2.38
#> 220  90 Ultisol 2.69
#> 221 120 Ultisol 2.89
#> 222 150 Ultisol 2.96
#> 223 210 Ultisol 3.23
#> 224 270 Ultisol 3.57
#> 225 365 Ultisol 3.79
#> 
#> $state
#>     POM     MOM     QOM      MB     DOM      EP      EM      IC     Tot 
#>  4.7100 17.6700  0.0000  0.8200  0.1480  0.0082  0.0082  0.0000 23.4840 
#> 
# Access the incubation data
memc_incubation_ultisol$data
#>     Day    Soil   IC
#> 181   1 Ultisol 0.12
#> 182   2 Ultisol 0.23
#> 183   4 Ultisol 0.37
#> 184   7 Ultisol 0.49
#> 185  10 Ultisol 0.77
#> 186  15 Ultisol 0.96
#> 187  31 Ultisol 1.65
#> 188  45 Ultisol 2.13
#> 189  60 Ultisol 2.46
#> 190  90 Ultisol 2.76
#> 191 120 Ultisol 3.07
#> 192 150 Ultisol 3.13
#> 193 210 Ultisol 3.48
#> 194 270 Ultisol 3.76
#> 195 365 Ultisol 3.99
#> 196   1 Ultisol 0.27
#> 197   2 Ultisol 0.40
#> 198   4 Ultisol 0.60
#> 199   7 Ultisol 0.71
#> 200  10 Ultisol 0.92
#> 201  15 Ultisol 1.01
#> 202  31 Ultisol 1.53
#> 203  45 Ultisol 1.80
#> 204  60 Ultisol 2.04
#> 205  90 Ultisol 2.23
#> 206 120 Ultisol 2.55
#> 207 150 Ultisol 2.64
#> 208 210 Ultisol 2.98
#> 209 270 Ultisol 3.30
#> 210 365 Ultisol 3.84
#> 211   1 Ultisol 0.20
#> 212   2 Ultisol 0.31
#> 213   4 Ultisol 0.50
#> 214   7 Ultisol 0.61
#> 215  10 Ultisol 0.83
#> 216  15 Ultisol 1.02
#> 217  31 Ultisol 1.81
#> 218  45 Ultisol 2.12
#> 219  60 Ultisol 2.38
#> 220  90 Ultisol 2.69
#> 221 120 Ultisol 2.89
#> 222 150 Ultisol 2.96
#> 223 210 Ultisol 3.23
#> 224 270 Ultisol 3.57
#> 225 365 Ultisol 3.79

# Access the initial state
memc_incubation_ultisol$state
#>     POM     MOM     QOM      MB     DOM      EP      EM      IC     Tot 
#>  4.7100 17.6700  0.0000  0.8200  0.1480  0.0082  0.0082  0.0000 23.4840 
```
