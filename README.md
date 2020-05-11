# R mrSOCbudget package
Package contains soil organic carbon cycling model based on IPCC Guidelines 2019 for data prepared in the madrat universe. 
[![Travis build status](https://travis-ci.com/pik-piam/mrcommons.svg?branch=master)](https://travis-ci.com/pik-piam/mrcommons)

## Purpose and Functionality

MadRat SOC budget package contains soil organic carbon (SOC) cycling model based on IPCC Guidelines 2019 for data prepared in the MadRat universe. 


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("mrSOCbudget")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Kristine Karstens <karstens@pik-potsdam.de>.

## Citation

citation("mrSOCbudget")
