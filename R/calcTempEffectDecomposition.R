#' @title calcTempEffectDecomposition
#' @description This function calculates the temperature effect on decomposition
#'              for mineral soils using the steady-state method (Tier 2) of the 2019
#'              Refinement to the 2006 IPP Guidelines for National Greenhouse Gas Inventories
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#' @param lpjml       Switch between LPJmL natveg versionstop
#' @param climatetype Switch between different climate scenarios
#'
#' @examples
#' \dontrun{
#' calcOutput("TempEffectDecomposition", aggregate = FALSE)
#' }
#'
#' @importFrom magpiesets findset

calcTempEffectDecomposition <- function(lpjml       = "LPJmL4_for_MAgPIE_44ac93de",
                                        climatetype = "GSWP3-W5E5:historical") {

  stage <- ifelse(grepl("historical", climatetype),
                  yes = "smoothed",
                  no  = "harmonized2020")

  param     <- readSource("IPCCSoil", convert = FALSE)
  paramTmax <- param[, , "tmax"]
  paramTopt <- param[, , "topt"]
  paramTa   <- param[, , "ta"]
  paramTb   <- param[, , "tb"]
  cellTemp  <- calcOutput("LPJmLClimateInput", climatetype  = climatetype,
                          variable     = "temperature:monthlyMean",
                          stage        = stage,
                          lpjmlVersion = lpjml,
                          aggregate    = FALSE)

  tempFunc  <- function(x) x**paramTa * exp(0.076 * (1 - x**paramTb))
  tempArg   <- (paramTmax - cellTemp) / (paramTmax - paramTopt)

  cellTmonthFactor  <- collapseNames(tempFunc(tempArg))
  cellTmonthFactor  <- ifelse(cellTemp > 45, 0, cellTmonthFactor)
  cellTfactor       <- dimSums(cellTmonthFactor, dim = 3) / 12

  return(list(x            = cellTfactor,
              weight       = NULL, # only cellular level supported
              unit         = "",
              description  = "Temperature effect on decomposition for mineral soils (unitless)",
              isocountries = FALSE))
}
