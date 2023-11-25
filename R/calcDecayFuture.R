#' @title calcDecayFuture
#' @description This function wraps together the decay rate for all SOC sub-pool per year
#'              for mineral soils using the steady-state method (Tier 2) of the 2019
#'              Refinement to the 2006 IPP Guidelines for National Greenhouse Gas Inventories
#'              for a given future climate scenario
#'              NOTE: This function only provides hamronized future climate scenario data used as
#'              input to MAgPIE. For historical data use \link{calcDecayRaw}
#'
#' @param lpjml       Switch between LPJmL natveg versionstop
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

calcDecayFuture <- function(lpjml       = "LPJmL4_for_MAgPIE_44ac93de",
                            climatetype = "GSWP3-W5E5:historical") {
  # only new mode on
  # - magpieInput
  # + harminzation and
  # weightingen (weight total carbon stocks)

  # Create settings for LPJmL/GCM from version and climatetype argument
  cfg   <- toolClimateInputVersion(lpjmlVersion = lpjml,
                                   climatetype = climatetype)

  decayBaselineHist <- toolSmooth(calcOutput("DecayRaw", aggregate = FALSE, lpjml = lpjml,
                                             climatetype = cfg$baselineHist, mode = "magpieInput"))
  decayBaselineGcm  <- toolSmooth(calcOutput("DecayRaw", aggregate = FALSE, lpjml = lpjml,
                                             climatetype = cfg$baselineGcm, mode = "magpieInput"))
  out <- toolHarmonize2Baseline(decayBaselineGcm, decayBaselineHist,
                                ref_year = cfg$refYearHist, method = "limited")

  if (cfg$climatetype != cfg$baselineGcm) {
    decayClimateScen <- toolSmooth(calcOutput("DecayRaw", aggregate = FALSE, lpjml = lpjml,
                                              climatetype = climatetype, mode = "magpieInput"))
    out <- toolHarmonize2Baseline(decayClimateScen, out,
                                  ref_year = cfg$refYearGcm, method = "limited")
  }
  # weightening
  # weight <- calcOutput("SoilCarbon", aggregate = FALSE) #nolint
  weight <- NULL

 return(list(x      = out,
             weight = weight,
             unit   = "per yr",
             description  = "Decay rate for all SOC sub-pool per year",
             isocountries = FALSE))
}
