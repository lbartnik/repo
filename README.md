Artifact Repository for R
==========================

| CRAN version    | Travis build status   | AppVeyor | Coverage |
| :-------------: |:---------------------:|:--------:|:--------:|
| [![CRAN version](http://www.r-pkg.org/badges/version/repository)](https://cran.r-project.org/package=repository) | [![Build Status](https://travis-ci.org/lbartnik/repository.svg?branch=master)](https://travis-ci.org/lbartnik/repository) | [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/lbartnik/repository?branch=master&svg=true)](https://ci.appveyor.com/project/lbartnik/repository) | [![codecov](https://codecov.io/gh/lbartnik/repository/branch/master/graph/badge.svg)](https://codecov.io/gh/lbartnik/repository)|


# Summary

Implements the middle layer between user interface and storage. Provides
calls to create a new repository, append artifacts to a repository,
search for artifacts given their tag values.


Other packages implementing the *repository of artifacts*:
  * [ui](https://github.com/lbartnik/ui) - text-based user interface
  * [storage](https://github.com/lbartnik/storage) - store of R objects
  * [defer](https://github.com/lbartnik/defer) - building and processing closures of R functions and data


# Installation

```r
install_github("lbartnik/defer")
install_github("lbartnik/storage")
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

