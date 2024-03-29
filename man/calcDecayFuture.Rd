% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcDecayFuture.R
\name{calcDecayFuture}
\alias{calcDecayFuture}
\title{calcDecayFuture}
\usage{
calcDecayFuture(
  lpjmlNatveg = "LPJmL4_for_MAgPIE_44ac93de",
  climatetype = "GSWP3-W5E5:historical"
)
}
\arguments{
\item{lpjmlNatveg}{Switch between LPJmL natveg versionstop}

\item{climatetype}{Switch between different climate scenarios}
}
\value{
magpie object in cellular or regional resolution for future climate scenarios
}
\description{
This function wraps together the decay rate for all SOC sub-pool per year
             for mineral soils using the steady-state method (Tier 2) of the 2019
             Refinement to the 2006 IPP Guidelines for National Greenhouse Gas Inventories
             for a given future climate scenario
             NOTE: This function only provides hamronized future climate scenario data used as
             input to MAgPIE. For historical data use \link{calcDecayRaw}
}
\examples{
\dontrun{
calcOutput("DecayFuture", aggregate = FALSE)
}

}
\seealso{
\code{\link{calcDecayRaw}}
}
\author{
Kristine Karstens
}
