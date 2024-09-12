#' @title calcTempEffectDecomposition
#' @description This function calculates the temperature effect on decomposition
#'              for mineral soils using the steady-state method (Tier 2) of the 2019
#'              Refinement to the 2006 IPP Guidelines for National Greenhouse Gas Inventories
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#' @param lpjmlNatveg Switch between LPJmL natveg versionstop
#' @param climatetype Switch between different climate scenarios
#'
#' @examples
#' \dontrun{
#' calcOutput("TempEffectDecomposition", aggregate = FALSE)
#' }
#'
#' @importFrom magpiesets findset

calcTempEffectDecomposition <- function(lpjmlNatveg = "LPJmL4_for_MAgPIE_44ac93de",
                                        climatetype = "GSWP3-W5E5:historical") {

  stage <- ifelse(grepl("historical", climatetype),
                  yes = "raw1901",
                  no  = "raw")

  param     <- readSource("IPCCSoil", convert = FALSE)
  paramTmax <- param[, , "tmax"]
  paramTopt <- param[, , "topt"]
  paramTa   <- param[, , "ta"]
  paramTb   <- param[, , "tb"]
  cellTemp  <- calcOutput("LPJmLClimateInput_new", climatetype  = climatetype,
                          variable     = "temperature:monthlyMean",
                          stage        = stage,
                          lpjmlVersion = lpjmlNatveg,
                          aggregate    = FALSE)

  tempFunc  <- function(x) x**paramTa * exp(0.076 * (1 - x**paramTb))
  tempArg   <- (paramTmax - cellTemp) / (paramTmax - paramTopt)

  cellTmonthFactor  <- collapseNames(tempFunc(tempArg))
  cellTmonthFactor  <- ifelse(cellTemp > 45, 0, cellTmonthFactor)
  cellTfactor       <- dimSums(cellTmonthFactor, dim = 3) / 12

  getSets(cellTfactor, fulldim = FALSE)[1] <- "x.y.iso"

  return(list(x            = cellTfactor,
              weight       = NULL, # only cellular level supported
              unit         = "",
              description  = "Temperature effect on decomposition for mineral soils (unitless)",
              isocountries = FALSE))
}
