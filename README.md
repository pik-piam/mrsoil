# MadRat Soil Organic Carbon Budget Library

R package **mrsoil**, version **1.4.0**

[![Travis build status](https://travis-ci.com/pik-piam/mrsoil.svg?branch=master)](https://travis-ci.com/pik-piam/mrsoil) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4317933.svg)](https://doi.org/10.5281/zenodo.4317933) 

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

Karstens K, Dietrich J (2020). _mrsoil: MadRat Soil Organic Carbon Budget Library_. doi: 10.5281/zenodo.4317933 (URL:
https://doi.org/10.5281/zenodo.4317933), R package version 1.4.0, <URL: https://github.com/pik-piam/mrsoil>.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {mrsoil: MadRat Soil Organic Carbon Budget Library},
  author = {Kristine Karstens and Jan Philipp Dietrich},
  year = {2020},
  note = {R package version 1.4.0},
  doi = {10.5281/zenodo.4317933},
  url = {https://github.com/pik-piam/mrsoil},
}
```

