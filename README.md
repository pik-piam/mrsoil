# MadRat Soil Organic Carbon Budget Library

R package **mrsoil**, version **1.7.3**

[![CRAN status](https://www.r-pkg.org/badges/version/mrsoil)](https://cran.r-project.org/package=mrsoil) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4317933.svg)](https://doi.org/10.5281/zenodo.4317933)  [![R build status](https://github.com/pik-piam/mrsoil/workflows/check/badge.svg)](https://github.com/pik-piam/mrsoil/actions) [![codecov](https://codecov.io/gh/pik-piam/mrsoil/branch/master/graph/badge.svg)](https://codecov.io/gh/pik-piam/mrsoil)

## Purpose and Functionality

This packages provides functions for soil organic carbon budget for mineral soils using the steady-state method (Tier 2) of the 2019 Refinement to the 2006 IPP Guidelines for National Greenhouse Gas Inventories.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("mrsoil")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Kristine Karstens <karstens@pik-potsdam.de>.

## Citation

To cite package **mrsoil** in publications use:

Karstens K, Dietrich J (2021). _mrsoil: MadRat Soil Organic Carbon Budget Library_. doi: 10.5281/zenodo.4317933 (URL:
https://doi.org/10.5281/zenodo.4317933), R package version 1.7.3, <URL: https://github.com/pik-piam/mrsoil>.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {mrsoil: MadRat Soil Organic Carbon Budget Library},
  author = {Kristine Karstens and Jan Philipp Dietrich},
  year = {2021},
  note = {R package version 1.7.3},
  doi = {10.5281/zenodo.4317933},
  url = {https://github.com/pik-piam/mrsoil},
}
```

