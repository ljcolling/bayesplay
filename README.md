
<!-- README.md is generated from README.Rmd. Please edit that file -->

# bayesplay: The Bayesian playground

<!-- badges: start -->

![R-CMD-check](https://github.com/ljcolling/bayesplay/workflows/R-CMD-check/badge.svg)
[![Codecov test
coverage](https://codecov.io/gh/ljcolling/bayesplay/branch/master/graph/badge.svg)](https://codecov.io/gh/ljcolling/bayesplay?branch=master)

<!-- badges: end -->

The goal of bayesplay is to provide an interface for calculating Bayes
factors for simple models. It does this in a way that makes the
calculations more *transparent* and it is therefore useful as a teaching
tools.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

    # install.packages("devtools")
    devtools::install_github("ljcolling/bayesplay")

## Basic usage

The `bayesplay` package comes with three basic functions for computing
Bayes factors.

1.  The `likelihood()` function for specifying likelihoods

2.  The `prior()` function for specifying priors

3.  And the `integral()` function

Currently the following distributions are supported for likelihoods and
priors

### Priors

1.  Normal distribution (`normal`)

2.  Uniform distribution (`uniform`)

3.  *t* distribution, including scaled and shifted and non-central *t*
    distributions (`student_t`)

4.  Cauchy distributions (`cauchy`)

### Likelihood

1.  Normal distribution (`normal`)

2.  *t* distribution, including scaled and shifted and non-central *t*
    distributions (`student_t`)

## Worked examples

For worked examples of the basic usage see [basic
usage](https://git.colling.net.nz/bayesplay/articles/basic.html).