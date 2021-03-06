
<!-- README.md is generated from README.Rmd. Please edit that file -->

# <img src="man/figures/shurp-hex.png" align="right" alt="" height="150" />

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![R-CMD-check](https://github.com/cathblatter/shurp2018/workflows/R-CMD-check/badge.svg)](https://github.com/cathblatter/shurp2018/actions)
<!-- badges: end -->

# shurp2018 🚧

The *shurp2018*-package was developed for developing reproducible
individual reports of data from the SHURP (Swiss Nursing Homes Human
Resources Project) study from 2018. More information about SHURP 2018 is
available here: <https://shurp.unibas.ch/>

The package is designed to only work with data of the SHURP2018 study
(not included here for data protection). In case anyone spots
inconsitencies in the code, suggestions are welcome.

## How to install and use the package?

Latest versions of R (<https://cran.r-project.org/>) and RStudio
(<https://www.rstudio.com/products/rstudio/download/#download>) are
needed.

``` r
# install the devtools-package to install from GitHub
install.packages("devtools")

# install the shurp2018-package from GitHub
devtools::install_github("cathblatter/shurp2018")

# once shurp2018 is installed, you just need to load it in every session
library(shurp2018)
```

### Disclaimer

This package is still in development mode - no guarantee on using the
functions. Please check manually if the results match your data!
