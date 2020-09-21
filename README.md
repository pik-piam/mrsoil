# Calculates historical Soil Organic Carbon Budget for Cropland

R package **mrSOCbudget**, version **0.8.4**

  

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
install.packages("mrSOCbudget")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Kristine Karstens <karstens@pik-potsdam.de>.

## Citation

To cite package **mrSOCbudget** in publications use:

Karstens K (2020). _mrSOCbudget: Calculates historical Soil Organic Carbon Budget for Cropland_. R package version 0.8.4.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {mrSOCbudget: Calculates historical Soil Organic Carbon Budget for Cropland},
  author = {Kristine Karstens},
  year = {2020},
  note = {R package version 0.8.4},
}
```

