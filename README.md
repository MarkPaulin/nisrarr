
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nisrarr

<!-- badges: start -->
<!-- badges: end -->

`nisrarr` is a package for accessing data from the [NISRA data
portal](https://data.nisra.gov.uk) directly from R.

## Installation

You can install the development version of nisrarr from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("MarkPaulin/nisrarr")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(nisrarr)
head(nisra_search(keyword = "claimant"))
```

``` r
head(nisra_load_dataset("CCMLGD"))
```
