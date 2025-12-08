# Example data, time series and initial conditions for Andisol from Wang et al. 2013 (10.1890/12-0681.1)

A list containing incubation data and initial states.

## Usage

``` r
memc_incubation_andisol
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

  :   Character indicating the soil type (e.g., "Andisol").

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
memc_incubation_andisol$data
#>    Day    Soil   IC
#> 1    1 Andisol 0.12
#> 2    2 Andisol 0.39
#> 3    4 Andisol 0.51
#> 4    7 Andisol 0.70
#> 5   10 Andisol 0.77
#> 6   15 Andisol 0.97
#> 7   31 Andisol 1.47
#> 8   45 Andisol 1.81
#> 9   60 Andisol 2.06
#> 10  90 Andisol 2.26
#> 11 120 Andisol 2.41
#> 12 150 Andisol 2.65
#> 13 210 Andisol 2.99
#> 14 270 Andisol 3.33
#> 15 365 Andisol 3.83
#> 16   1 Andisol 0.19
#> 17   2 Andisol 0.44
#> 18   4 Andisol 0.53
#> 19   7 Andisol 0.74
#> 20  10 Andisol 0.94
#> 21  15 Andisol 1.11
#> 22  31 Andisol 1.54
#> 23  45 Andisol 1.85
#> 24  60 Andisol 2.11
#> 25  90 Andisol 2.42
#> 26 120 Andisol 2.72
#> 27 150 Andisol 2.96
#> 28 210 Andisol 3.35
#> 29 270 Andisol 3.70
#> 30 365 Andisol 4.21
#> 31   1 Andisol 0.16
#> 32   2 Andisol 0.27
#> 33   4 Andisol 0.39
#> 34   7 Andisol 0.71
#> 35  10 Andisol 0.83
#> 36  15 Andisol 0.94
#> 37  31 Andisol 1.25
#> 38  45 Andisol 1.53
#> 39  60 Andisol 1.81
#> 40  90 Andisol 2.06
#> 41 120 Andisol 2.36
#> 42 150 Andisol 2.58
#> 43 210 Andisol 2.87
#> 44 270 Andisol 3.20
#> 45 365 Andisol 3.65

# Access the initial state
memc_incubation_andisol$state
#>    POM    MOM    QOM     MB    DOM     EP     EM     IC    Tot 
#> 20.060 64.860  0.000  0.860  0.640  0.086  0.086  0.000 86.592 
```
