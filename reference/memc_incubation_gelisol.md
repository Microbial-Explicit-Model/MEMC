# Example data, time series and initial conditions for Gelisol from Wang et al. 2013 (10.1890/12-0681.1)

A list containing incubation data and initial states.

## Usage

``` r
memc_incubation_gelisol
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

  :   Character indicating the soil type (e.g., "Gelisol").

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
memc_incubation_gelisol$data
#>    Day    Soil   IC
#> 46   1 Gelisol 0.27
#> 47   2 Gelisol 0.36
#> 48   4 Gelisol 0.42
#> 49   7 Gelisol 0.52
#> 50  10 Gelisol 0.75
#> 51  15 Gelisol 0.91
#> 52  31 Gelisol 1.31
#> 53  45 Gelisol 1.55
#> 54  60 Gelisol 1.67
#> 55  90 Gelisol 1.75
#> 56 120 Gelisol 1.92
#> 57 150 Gelisol 1.98
#> 58 210 Gelisol 2.21
#> 59 270 Gelisol 2.37
#> 60 365 Gelisol 2.58
#> 61   1 Gelisol 0.18
#> 62   2 Gelisol 0.34
#> 63   4 Gelisol 0.44
#> 64   7 Gelisol 0.53
#> 65  10 Gelisol 0.76
#> 66  15 Gelisol 1.14
#> 67  31 Gelisol 1.42
#> 68  45 Gelisol 1.54
#> 69  60 Gelisol 1.76
#> 70  90 Gelisol 1.92
#> 71 120 Gelisol 1.97
#> 72 150 Gelisol 1.98
#> 73 210 Gelisol 2.23
#> 74 270 Gelisol 2.36
#> 75 365 Gelisol 2.48
#> 76   1 Gelisol 0.15
#> 77   2 Gelisol 0.36
#> 78   4 Gelisol 0.55
#> 79   7 Gelisol 0.73
#> 80  10 Gelisol 0.92
#> 81  15 Gelisol 1.05
#> 82  31 Gelisol 1.38
#> 83  45 Gelisol 1.48
#> 84  60 Gelisol 1.69
#> 85  90 Gelisol 1.85
#> 86 120 Gelisol 2.03
#> 87 150 Gelisol 2.15
#> 88 210 Gelisol 2.35
#> 89 270 Gelisol 2.54
#> 90 365 Gelisol 2.71

# Access the initial state
memc_incubation_gelisol$state
#>    POM    MOM    QOM     MB    DOM     EP     EM     IC    Tot 
#>  4.250 11.040  0.000  0.050  0.170  0.005  0.005  0.000 15.520 
```
