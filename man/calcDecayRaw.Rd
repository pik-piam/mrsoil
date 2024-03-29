% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcDecayRaw.R
\name{calcDecayRaw}
\alias{calcDecayRaw}
\title{calcDecayRaw}
\usage{
calcDecayRaw(
  lpjmlNatveg = "LPJmL4_for_MAgPIE_44ac93de",
  climatetype = "GSWP3-W5E5:historical",
  mode = "historicalSpinup"
)
}
\arguments{
\item{lpjmlNatveg}{Switch between LPJmL natveg versionstop}

\item{climatetype}{Switch between different climate scenarios}

\item{mode}{"historicalSpinup" for historical period and
"magpieInput" for future}
}
\value{
magpie object in cellular resolution
}
\description{
This function wraps together the decay rate for all SOC sub-pool per year
             for mineral soils using the steady-state method (Tier 2) of the 2019
             Refinement to the 2006 IPP Guidelines for National Greenhouse Gas Inventories
}
\examples{
\dontrun{
calcOutput("Decay", aggregate = FALSE)
}

}
\author{
Kristine Karstens
}
