# Example data, time series and initial conditions for Mollisol from Wang et al. 2013 (10.1890/12-0681.1)

A list containing incubation data and initial states.

## Usage

``` r
memc_incubation_mollisol
```

## Format

A list of 2 elements:

- `data`:

  data frame of time-series data for carbon pools from the incubation
  experiment. The data frame contains the following variables:

  `Day`

  :   numeric, indicating the time since the start of the incubation
      experiment.

  `Soil`

  :   Character indicating the soil type (e.g., "Mollisol").

  `IC`

  :   Numeric value for inorganic carbon (mg C g\\^{-1}\\ soil).

- `state`:

  named numeric vector of initial state values for the soil carbon
  pools.

## Details

The \`data\` element contains time-series observations, while the
\`state\` element represents the initial conditions for the soil carbon
pools.

## Examples

``` r
# Access the incubation data
memc_incubation_mollisol$data
#>     Day     Soil   IC
#> 91    1 Mollisol 0.19
#> 92    2 Mollisol 0.42
#> 93    4 Mollisol 0.61
#> 94    7 Mollisol 0.76
#> 95   10 Mollisol 0.86
#> 96   15 Mollisol 1.08
#> 97   31 Mollisol 1.82
#> 98   45 Mollisol 2.24
#> 99   60 Mollisol 2.48
#> 100  90 Mollisol 2.81
#> 101 120 Mollisol 3.19
#> 102 150 Mollisol 3.44
#> 103 210 Mollisol 3.77
#> 104 270 Mollisol 4.08
#> 105 365 Mollisol 4.51
#> 106   1 Mollisol 0.13
#> 107   2 Mollisol 0.38
#> 108   4 Mollisol 0.59
#> 109   7 Mollisol 0.68
#> 110  10 Mollisol 0.79
#> 111  15 Mollisol 0.99
#> 112  31 Mollisol 1.57
#> 113  45 Mollisol 1.95
#> 114  60 Mollisol 2.27
#> 115  90 Mollisol 2.53
#> 116 120 Mollisol 2.83
#> 117 150 Mollisol 3.15
#> 118 210 Mollisol 3.43
#> 119 270 Mollisol 3.80
#> 120 365 Mollisol 4.59
#> 121   1 Mollisol 0.08
#> 122   2 Mollisol 0.76
#> 123   4 Mollisol 0.94
#> 124   7 Mollisol 1.15
#> 125  10 Mollisol 1.38
#> 126  15 Mollisol 1.62
#> 127  31 Mollisol 2.03
#> 128  45 Mollisol 2.47
#> 129  60 Mollisol 2.92
#> 130  90 Mollisol 3.29
#> 131 120 Mollisol 3.61
#> 132 150 Mollisol 3.84
#> 133 210 Mollisol 4.14
#> 134 270 Mollisol 4.51
#> 135 365 Mollisol 4.89

# Access the initial state
memc_incubation_mollisol$state
#>    POM    MOM    QOM     MB    DOM     EP     EM     IC    Tot 
#>  3.250 27.970  0.000  0.640  0.210  0.064  0.064  0.000 32.198 
```
