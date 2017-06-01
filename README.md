---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- rmarkdown v1 --> <!--to accomodate pandoc bug on windows-->



# hansard: Accessing Westminster Parliament Data
[![license](https://img.shields.io/github/license/mashape/apistatus.svg)]()
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/hansard)](https://cran.r-project.org/package=hansard)
[![GitHub tag](https://img.shields.io/github/tag/evanodell/hansard.svg)](https://github.com/evanodell/hansard)
[![](http://cranlogs.r-pkg.org/badges/grand-total/hansard)](https://dgrtwo.shinyapps.io/cranview/)
[![Build Status](https://travis-ci.org/EvanOdell/hansard.png?branch=master)](https://travis-ci.org/EvanOdell/hansard) 
[![DOI](https://zenodo.org/badge/72111315.svg)](https://zenodo.org/badge/latestdoi/72111315)
[![Coverage Status](https://img.shields.io/codecov/c/github/EvanOdell/hansard/master.svg)](https://codecov.io/github/EvanOdell/hansard?branch=master)

An R package to automatically fetch data from the UK Parliament API. Ironically, Hansard data is not yet accessible through the API. To explore all available data see <http://www.data.parliament.uk/>. Help on the API itself is here: <http://api.data.parliament.uk/help/>. 

Like the UK parliament API, this package is a work in progress. Additional functionalities will be added to the package as they are developed in the API. The most up-to-date documentation for this package will always be available at <http://ropengov.github.io/hansard> and at <http://evanodell.com/hansard/>.

To install from CRAN run:

```
install.packages("hansard")
```

To install the development version run:

```
install.packages("devtools")
devtools::install_github("EvanOdell/hansard")
```

## Using hansard

For an introduction to `hansard`, please see the [vignette](http://ropengov.github.io/hansard/articles/introduction.html). If you are unfamiliar with APIs [https://zapier.com/learn/apis/](this introductory course) is useful.

### Tests and test coverage

The `hansard` package includes extensive tests of the different API functions. However, to reduce the impact of repeated testing and calls to the API, these tests have been disabled for both CRAN and Travis. To run these tests locally please download the development version of `hansard`.

#### API calls with console input

Previous versions (<=0.3.4) of `hansard` included options for console-based inputs to call data and interact with the API. This feature has been removed from subsequent versions, and is available in the [https://github.com/EvanOdell/hansard-console](`hansardconsole`) package. There are no plans to submit this to CRAN, and it is not being actively maintained.

### Disclaimer

This package is in no way officially related to or endorsed by the UK Parliamentary Data Service.

### Acknowledgements

This package is part of [rOpenGov](http://ropengov.github.io). It was written by [Evan Odell](http://evanodell.com).
