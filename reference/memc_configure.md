# Set up a model configuration

Set up a model configuration

## Usage

``` r
memc_configure(
  params,
  state,
  name = "unnamed",
  DOMuptake = "MM",
  POMdecomp = "MM",
  MBdecay = "LM"
)
```

## Arguments

- params:

  data.table containing the following columns: parameter, value, and
  units.

- state:

  a vector of the initial state values, must be named

- name:

  string name of the model configuration, default set to "MEND".

- DOMuptake:

  string indicating the dynamics used to model microbial decomposition
  of DOM, one of the following "MM", "RMM", or "ECA"

- POMdecomp:

  string indicating the dynamics used to model microbial decomposition
  of POM, one of the following "MM", "RMM", "ECA", or "LM"

- MBdecay:

  string indicating microbial decay, one of the following ""LM" or "DD"

## Value

memc_single_config object of the name, dynamics, parameters and starting
state values

## See also

Other helper functions:
[`memc_update_config()`](https://microbial-explicit-model.github.io/MEMC/reference/memc_update_config.md),
[`memc_update_params()`](https://microbial-explicit-model.github.io/MEMC/reference/memc_update_params.md),
[`update_state()`](https://microbial-explicit-model.github.io/MEMC/reference/update_state.md)

## Examples

``` r
# Modify the MEND model
m <- MEND_config
m_mod <- memc_configure(m$params, m$state, "MEND_modified", POMdecomp = "LM")
summary(m_mod)
#> 
#> 
#> |model         |DOMuptake |POMdecomp |MBdecay |
#> |:-------------|:---------|:---------|:-------|
#> |MEND_modified |MM        |LM        |LM      |
memc_solve(m_mod, 0:10)
#>      time variable        value       units          name
#>     <num>   <fctr>        <num>      <char>        <char>
#>  1:     0      POM 1.000000e+01 mg C/g soil MEND_modified
#>  2:     1      POM 7.813399e-02 mg C/g soil MEND_modified
#>  3:     2      POM 5.717875e-02 mg C/g soil MEND_modified
#>  4:     3      POM 2.533637e-02 mg C/g soil MEND_modified
#>  5:     4      POM 1.094505e-02 mg C/g soil MEND_modified
#>  6:     5      POM 4.727821e-03 mg C/g soil MEND_modified
#>  7:     6      POM 2.042632e-03 mg C/g soil MEND_modified
#>  8:     7      POM 8.828356e-04 mg C/g soil MEND_modified
#>  9:     8      POM 3.815859e-04 mg C/g soil MEND_modified
#> 10:     9      POM 1.650119e-04 mg C/g soil MEND_modified
#> 11:    10      POM 7.138747e-05 mg C/g soil MEND_modified
#> 12:     0      MOM 5.000000e+00 mg C/g soil MEND_modified
#> 13:     1      MOM 1.052746e+01 mg C/g soil MEND_modified
#> 14:     2      MOM 1.102035e+01 mg C/g soil MEND_modified
#> 15:     3      MOM 1.129614e+01 mg C/g soil MEND_modified
#> 16:     4      MOM 1.141555e+01 mg C/g soil MEND_modified
#> 17:     5      MOM 1.146675e+01 mg C/g soil MEND_modified
#> 18:     6      MOM 1.148849e+01 mg C/g soil MEND_modified
#> 19:     7      MOM 1.149749e+01 mg C/g soil MEND_modified
#> 20:     8      MOM 1.150101e+01 mg C/g soil MEND_modified
#> 21:     9      MOM 1.150214e+01 mg C/g soil MEND_modified
#> 22:    10      MOM 1.150226e+01 mg C/g soil MEND_modified
#> 23:     0      QOM 1.000000e-01 mg C/g soil MEND_modified
#> 24:     1      QOM 1.214956e-01 mg C/g soil MEND_modified
#> 25:     2      QOM 1.267735e-01 mg C/g soil MEND_modified
#> 26:     3      QOM 1.273939e-01 mg C/g soil MEND_modified
#> 27:     4      QOM 1.279766e-01 mg C/g soil MEND_modified
#> 28:     5      QOM 1.285589e-01 mg C/g soil MEND_modified
#> 29:     6      QOM 1.291415e-01 mg C/g soil MEND_modified
#> 30:     7      QOM 1.297245e-01 mg C/g soil MEND_modified
#> 31:     8      QOM 1.303082e-01 mg C/g soil MEND_modified
#> 32:     9      QOM 1.308926e-01 mg C/g soil MEND_modified
#> 33:    10      QOM 1.314779e-01 mg C/g soil MEND_modified
#> 34:     0       MB 2.000000e+00 mg C/g soil MEND_modified
#> 35:     1       MB 1.845488e+00 mg C/g soil MEND_modified
#> 36:     2       MB 1.294029e+00 mg C/g soil MEND_modified
#> 37:     3       MB 5.671012e-01 mg C/g soil MEND_modified
#> 38:     4       MB 2.449693e-01 mg C/g soil MEND_modified
#> 39:     5       MB 1.058185e-01 mg C/g soil MEND_modified
#> 40:     6       MB 4.571948e-02 mg C/g soil MEND_modified
#> 41:     7       MB 1.975894e-02 mg C/g soil MEND_modified
#> 42:     8       MB 8.541734e-03 mg C/g soil MEND_modified
#> 43:     9       MB 3.693755e-03 mg C/g soil MEND_modified
#> 44:    10       MB 1.598015e-03 mg C/g soil MEND_modified
#> 45:     0      DOM 1.000000e+00 mg C/g soil MEND_modified
#> 46:     1      DOM 2.243737e+00 mg C/g soil MEND_modified
#> 47:     2      DOM 1.501442e-01 mg C/g soil MEND_modified
#> 48:     3      DOM 1.075336e-01 mg C/g soil MEND_modified
#> 49:     4      DOM 1.073661e-01 mg C/g soil MEND_modified
#> 50:     5      DOM 1.074323e-01 mg C/g soil MEND_modified
#> 51:     6      DOM 1.075450e-01 mg C/g soil MEND_modified
#> 52:     7      DOM 1.076909e-01 mg C/g soil MEND_modified
#> 53:     8      DOM 1.078614e-01 mg C/g soil MEND_modified
#> 54:     9      DOM 1.080478e-01 mg C/g soil MEND_modified
#> 55:    10      DOM 1.082434e-01 mg C/g soil MEND_modified
#> 56:     0       EP 1.000000e-05 mg C/g soil MEND_modified
#> 57:     1       EP 2.312644e-02 mg C/g soil MEND_modified
#> 58:     2       EP 4.279750e-02 mg C/g soil MEND_modified
#> 59:     3       EP 5.337674e-02 mg C/g soil MEND_modified
#> 60:     4       EP 5.792604e-02 mg C/g soil MEND_modified
#> 61:     5       EP 5.985625e-02 mg C/g soil MEND_modified
#> 62:     6       EP 6.065530e-02 mg C/g soil MEND_modified
#> 63:     7       EP 6.096581e-02 mg C/g soil MEND_modified
#> 64:     8       EP 6.106531e-02 mg C/g soil MEND_modified
#> 65:     9       EP 6.107364e-02 mg C/g soil MEND_modified
#> 66:    10       EP 6.104261e-02 mg C/g soil MEND_modified
#> 67:     0       EM 1.000000e-05 mg C/g soil MEND_modified
#> 68:     1       EM 2.312644e-02 mg C/g soil MEND_modified
#> 69:     2       EM 4.279750e-02 mg C/g soil MEND_modified
#> 70:     3       EM 5.337674e-02 mg C/g soil MEND_modified
#> 71:     4       EM 5.792604e-02 mg C/g soil MEND_modified
#> 72:     5       EM 5.985625e-02 mg C/g soil MEND_modified
#> 73:     6       EM 6.065530e-02 mg C/g soil MEND_modified
#> 74:     7       EM 6.096581e-02 mg C/g soil MEND_modified
#> 75:     8       EM 6.106531e-02 mg C/g soil MEND_modified
#> 76:     9       EM 6.107364e-02 mg C/g soil MEND_modified
#> 77:    10       EM 6.104261e-02 mg C/g soil MEND_modified
#> 78:     0       IC 0.000000e+00 mg C/g soil MEND_modified
#> 79:     1       IC 3.237455e+00 mg C/g soil MEND_modified
#> 80:     2       IC 5.365950e+00 mg C/g soil MEND_modified
#> 81:     3       IC 5.869766e+00 mg C/g soil MEND_modified
#> 82:     4       IC 6.077359e+00 mg C/g soil MEND_modified
#> 83:     5       IC 6.167019e+00 mg C/g soil MEND_modified
#> 84:     6       IC 6.205775e+00 mg C/g soil MEND_modified
#> 85:     7       IC 6.222536e+00 mg C/g soil MEND_modified
#> 86:     8       IC 6.229789e+00 mg C/g soil MEND_modified
#> 87:     9       IC 6.232929e+00 mg C/g soil MEND_modified
#> 88:    10       IC 6.234288e+00 mg C/g soil MEND_modified
#> 89:     0      Tot 1.810002e+01 mg C/g soil MEND_modified
#> 90:     1      Tot 1.486256e+01 mg C/g soil MEND_modified
#> 91:     2      Tot 1.273407e+01 mg C/g soil MEND_modified
#> 92:     3      Tot 1.223025e+01 mg C/g soil MEND_modified
#> 93:     4      Tot 1.202266e+01 mg C/g soil MEND_modified
#> 94:     5      Tot 1.193300e+01 mg C/g soil MEND_modified
#> 95:     6      Tot 1.189424e+01 mg C/g soil MEND_modified
#> 96:     7      Tot 1.187748e+01 mg C/g soil MEND_modified
#> 97:     8      Tot 1.187023e+01 mg C/g soil MEND_modified
#> 98:     9      Tot 1.186709e+01 mg C/g soil MEND_modified
#> 99:    10      Tot 1.186573e+01 mg C/g soil MEND_modified
#>      time variable        value       units          name
```
