% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcCarbonInputMultiplier.R
\name{calcCarbonInputMultiplier}
\alias{calcCarbonInputMultiplier}
\title{calcCarbonInputMulitplier}
\usage{
calcCarbonInputMultiplier(inputType = "kcr")
}
\arguments{
\item{inputType}{switch between 'kcr' (default), or 'kli', 'generic'}
}
\value{
magpie object on choosen resolution
}
\description{
This function compiles carbon inputs multipliers for steady state calculations for mineral soils
             using the steady-state method (Tier 2) of the 2019 Refinement to the 2006 IPP Guidelines
             for National Greenhouse Gas Inventories
}
\examples{
\dontrun{
calcOutput("CarbonInputMulitplier", aggregate = FALSE, input = generic)
}

}
\author{
Kristine Karstens
}
