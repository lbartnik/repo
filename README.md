Artifact Repository for R
==========================

| CRAN version    | Travis build status   | AppVeyor | Coverage |
| :-------------: |:---------------------:|:--------:|:--------:|
| [![CRAN version](http://www.r-pkg.org/badges/version/repository)](https://cran.r-project.org/package=repository) | [![Build Status](https://travis-ci.org/lbartnik/repository.svg?branch=master)](https://travis-ci.org/lbartnik/repository) | [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/lbartnik/repository?branch=master&svg=true)](https://ci.appveyor.com/project/lbartnik/repository) | [![codecov](https://codecov.io/gh/lbartnik/repository/branch/master/graph/badge.svg)](https://codecov.io/gh/lbartnik/repository)|


# Summary

`repository` is the middle layer of the __Repository of Artifacts__. It
contains the logic to store, search and process artifacts. The other
packages are:

  * [ui](https://github.com/lbartnik/ui) - text-based user interface
  * [storage](https://github.com/lbartnik/storage) - store of R objects
  * [defer](https://github.com/lbartnik/defer) - building and processing closures of R functions and data
  * [utilities](https://github.com/lbartnik/utilities) - a set of shared utility functions


# FAQ

**What is the use case for the repository of artifacts?**

The goal is to build a generic mechanism to store artifacts of data
analysis and to search and browse such repository. Effectively, this
will extend R's history mechanism (see `?utils::history`) to contain
not only commands but also objects, data sets, plots, printouts, etc.
Thus, there is no domain-specific use case other than extending a basic
mechanism with the hope that users will adopt it according to their
personal preferences and working styles.


**What are the key aspects of the repository of artifacts?**

  1. Store evey artifact present in any of the R sessions within a data
     analysis project.
  1. Provide a search mechanism to narrow the selection of artifacts and
     identify the artifact of interest.
  1. Provide an interactive artifact browser to recall the context of any
     given artifact.

My estimate is that the current implementation covers 60% of the first
point, 45% of the second point, and about 10-15% of the third one. See
the [work plan](https://lbartnik.github.io/repository/) for further
information.


# Installation

```r
install_github("lbartnik/defer")
install_github("lbartnik/storage")
install_github("lbartnik/utilities")
install_github("lbartnik/repository")
install_github("lbartnik/ui")
```

```r
library(ui)
```


# Documentation

  * [user interface tutorial](https://lbartnik.github.io/ui/) - ways and APIs to interact with the repository
  * [work plan](https://lbartnik.github.io/repository/) - current state of affairs and ideas for research
  * [graphical artifact browser](https://lbartnik.github.io/experiment/) - an older attempt at browsing artifacts via Shiny and JS

