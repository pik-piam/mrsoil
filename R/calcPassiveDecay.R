#' @title calcPassiveDecay
#' @description This function calculates the decay rate for passive SOC sub-pool per year for mineral soils
#' using the steady-state method (Tier 2) of the 2019 Refinement to the 2006 IPP Guidelines
#' for National Greenhouse Gas Inventories
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#' @param landtype 'crop' for cropland, 'natveg' for all the rest
#' @examples
#' \dontrun{ calcOutput("PassiveDecay", aggregate = FALSE) }
#'
#' @import madrat
#' @import magclass

calcPassiveDecay <- function(landtype="crop") {

  landtype2irrigation <- c(crop   = "mixed",
                           natveg = "rainfed")

  param <- readSource("IPCCSoil", convert=FALSE)
  param.k_popt    <- setYears(param[,,"kfacp"], NULL) # decay rate under optimum condition for passive (k5)
  cell.w_Factor   <- calcOutput("WaterEffectDecomposition", irrigation=landtype2irrigation[landtype], aggregate = FALSE)
  cell.t_Factor   <- calcOutput("TempEffectDecomposition", aggregate = FALSE)

  cell.k_passive  <- param.k_popt * cell.w_Factor * cell.t_Factor

  return(list(
    x=cell.k_passive,
    weight=NULL,
    unit="per yr",
    description="Decay rate for passive SOC sub-pool per year",
    isocountries=FALSE))
}
