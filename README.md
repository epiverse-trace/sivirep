
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sivirep

*sivirep* provides functions for data wrangling and automated reports from *SIVIGILA* source.

<!-- badges: start -->

[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R-CMD-check](https://github.com/epiverse-trace/readepi/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/epiverse-trace/readepi/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/epiverse-trace/readepi/branch/main/graph/badge.svg)](https://app.codecov.io/gh/epiverse-trace/readepi?branch=main)
[![lifecycle-concept](https://raw.githubusercontent.com/reconverse/reconverse.github.io/master/images/badge-concept.svg)](https://www.reconverse.org/lifecycle.html#concept)
<!-- badges: end -->

## Installation

You can install the development version of *sivirep* from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("TRACE-LAC/sivirep")
```

## Example

These examples illustrate some of the current functionalities:

``` r
library(sivirep)

# example of read_path_data():
# (Function that imports data through an URL or path)
data_url <- "some_path_here"
read_path_data(data_url)
```

## Development

### Lifecycle

This package is currently a *concept*, as defined by the [RECON software
lifecycle](https://www.reconverse.org/lifecycle.html). This means that
essential features and mechanisms are still being developed, and the
package is not ready for use outside of the development team.

### Contributions

Contributions are welcome via [pull
requests](https://github.com/epiverse-trace/readepi/pulls).

Contributors to the project include:

  - Zulma M. Cucunubá (author)
  - Geraldine Gómez Millán (contributor)

### Code of Conduct

Please note that the linelist project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.