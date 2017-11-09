<!-- README.md is generated from README.Rmd. Please edit that file -->



frame
=====

[![Build Status (Linux)][travis-badge]][travis]
[![Build Status (Windows)][appveyor-badge]][appveyor]
[![Coverage Status][codecov-badge]][codecov]
[![CRAN Status][cran-badge]][cran]
[![License][apache-badge]][apache]
[![CRAN RStudio Mirror Downloads][cranlogs-badge]][cran]


*frame* is an R package providing an extension to the `data.frame` type
that makes it easy to affix sampling frames to variables. It also provides
a nice print function than the `data.frame` default.


Installation
------------

### Stable version

*frame* is [available on CRAN][cran]. To install the latest released version,
run the following command in R:

```r
### install.packages("frame") # not yet, actually
```

### Development version

To install the latest development version, run the following:

```r
devtools::install_github("patperry/r-frame")
```


Usage
-----


Citation
--------

Cite *utf8* with the following BibTeX entry:

    @Manual{,
        title = {frame: Data with Context},
        author = {Patrick O. Perry},
        year = {2017},
        note = {R package version 0.0.0},
    }


Contributing
------------

The project maintainer welcomes contributions in the form of feature requests,
bug reports, comments, unit tests, vignettes, or other code.  If you'd like to
contribute, either

 + fork the repository and submit a pull request (note the nonstandard
   instructions for [building from source][building]);

 + [file an issue][issues];

 + or contact the maintainer via e-mail.

This project is released with a [Contributor Code of Conduct][conduct],
and if you choose to contribute, you must adhere to its terms.


[apache]: https://www.apache.org/licenses/LICENSE-2.0.html "Apache License, Version 2.0"
[apache-badge]: https://img.shields.io/badge/License-Apache%202.0-blue.svg "Apache License, Version 2.0"
[appveyor]: https://ci.appveyor.com/project/patperry/r-frame/branch/master "Continuous Integration (Windows)"
[appveyor-badge]: https://ci.appveyor.com/api/projects/status/github/patperry/r-frame?branch=master&svg=true "Continuous Inegration (Windows)"
[building]: #development-version "Building from Source"
[codecov]: https://codecov.io/github/patperry/r-frame?branch=master "Code Coverage"
[codecov-badge]: https://codecov.io/github/patperry/r-frame/coverage.svg?branch=master "Code Coverage"
[conduct]: https://github.com/patperry/r-frame/blob/master/CONDUCT.md "Contributor Code of Conduct"
[cran]: https://cran.r-project.org/package=frame "CRAN Page"
[cran-badge]: http://www.r-pkg.org/badges/version/frame "CRAN Page"
[cranlogs-badge]: http://cranlogs.r-pkg.org/badges/frame "CRAN Downloads"
[issues]: https://github.com/patperry/r-frame/issues "Issues"
[travis]: https://travis-ci.org/patperry/r-frame "Continuous Integration (Linux)"
[travis-badge]: https://api.travis-ci.org/patperry/r-frame.svg?branch=master "Continuous Integration (Linux)"
