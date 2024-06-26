#' @title calcDecayFuture
#' @description This function wraps together the decay rate for all SOC sub-pool per year
#'              for mineral soils using the steady-state method (Tier 2) of the 2019
#'              Refinement to the 2006 IPP Guidelines for National Greenhouse Gas Inventories
#'              for a given future climate scenario
#'              NOTE: This function only provides hamronized future climate scenario data used as
#'              input to MAgPIE. For historical data use \link{calcDecayRaw}
#'
#' @param lpjmlNatveg Switch between LPJmL natveg versionstop
#' @param climatetype Switch between different climate scenarios
#'
#' @return magpie object in cellular or regional resolution for future climate scenarios
#' @author Kristine Karstens
#'
#' @seealso
#' \code{\link{calcDecayRaw}}
#'
#' @examples
#' \dontrun{
#' calcOutput("DecayFuture", aggregate = FALSE)
#' }
#'
#' @import madrat
#' @import magclass
#' @importFrom mstools toolSmooth toolHarmonize2Baseline

calcDecayFuture <- function(lpjmlNatveg = "LPJmL4_for_MAgPIE_44ac93de",
                            climatetype = "GSWP3-W5E5:historical") {
  # Create settings for LPJmL/GCM from version and climatetype argument
  cfg   <- toolClimateInputVersion(lpjmlVersion = lpjmlNatveg,
                                   climatetype  = climatetype)

  decayBaselineHist <- toolSmooth(calcOutput("DecayRaw", aggregate = FALSE, lpjmlNatveg = lpjmlNatveg,
                                             climatetype = cfg$baselineHist, mode = "magpieInput"))
  decayBaselineGcm  <- toolSmooth(calcOutput("DecayRaw", aggregate = FALSE, lpjmlNatveg = lpjmlNatveg,
                                             climatetype = cfg$baselineGcm, mode = "magpieInput"))
  out <- toolHarmonize2Baseline(decayBaselineGcm, decayBaselineHist,
                                ref_year = cfg$refYearHist, method = "limited")

  if (cfg$climatetype != cfg$baselineGcm) {
    decayClimateScen <- toolSmooth(calcOutput("DecayRaw", aggregate = FALSE, lpjmlNatveg = lpjmlNatveg,
                                              climatetype = climatetype, mode = "magpieInput"))
    out <- toolHarmonize2Baseline(decayClimateScen, out,
                                  ref_year = cfg$refYearGcm, method = "limited")
  }

  weight <- collapseDim(calcOutput("SoilCarbon", aggregate = FALSE, years = "y1995", output = "actualstate",
                                   lpjmlNatveg = lpjmlNatveg, climatetype = cfg$baselineHist))
  weight <- mbind(add_dimension(collapseDim(weight[, , "crop"] + 10^(-10)),
                                dim = 3.2,  add = "tillage", nm = "fulltill"),
                  add_dimension(collapseDim(weight[, , "crop"] + 10^(-10)),
                                dim = 3.2,  add = "tillage", nm = "reducedtill"),
                  add_dimension(collapseDim(weight[, , "natveg"] + 10^(-10)),
                                dim = 3.2,  add = "tillage", nm = "notill"))

  getSets(out,    fulldim = FALSE)[1] <- "x.y.iso"
  getSets(weight, fulldim = FALSE)[1] <- "x.y.iso"
  return(list(x      = out,
              weight = weight,
              unit   = "per yr",
              description  = "Decay rate for all SOC sub-pool per year",
              isocountries = FALSE))
}
