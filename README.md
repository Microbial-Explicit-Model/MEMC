# IN DEVELOPMENT!

[![R-CMD-check](https://github.com/Microbial-Explicit-Model/MEMC/actions/workflows/test_rcmd.yml/badge.svg)](https://github.com/Microbial-Explicit-Model/MEMC/actions/workflows/test_rcmd.yml)
[![codecov](https://codecov.io/gh/kdorheim/MEMC/branch/main/graph/badge.svg?token=ia2NHHtpJs)](https://codecov.io/gh/kdorheim/MEMC)
[![Documentation](https://github.com/Microbial-Explicit-Model/MEMC/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/Microbial-Explicit-Model/MEMC/actions/workflows/pkgdown.yaml)


# About

`MEMC` is a R package representation of different microbial explicit carbon models in a flexible framework for model comparison exercises. 


## Getting Started Using `MEMC`

Follow the download and installation insturctions for [R](https://cloud.r-project.org/) and [R studio](https://www.rstudio.com/products/rstudio/download/). 


#### For developers

Clone the repository to install the package in development mode using the `devtools::load_all()` call in R studio from the root directory. Please refer to [r-pkgs](https://r-pkgs.org/) for more information on how to build and develop R packages. 


#### For users

Use `remotes` to install MEMC as a built R package directory from github. 

```
# use install.packages("remotes") to install this package the first time.
library(remotes)

# Now build and install the R package on your local machine.
install_github('Microbial-Explicit-Model/MEMC') 
```








