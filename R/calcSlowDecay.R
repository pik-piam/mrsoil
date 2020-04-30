#' @title calcSlowDecay
#' @description This function calculates the decay rate for slow SOC sub-pool per year for mineral soils
#' using the steady-state method (Tier 2) of the 2019 Refinement to the 2006 IPP Guidelines
#' for National Greenhouse Gas Inventories
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#' @param landtype 'crop' for cropland, 'natveg' for all the rest
#' @examples
#' \dontrun{ calcOutput("SlowDecay", aggregate = FALSE) }
#'
#' @import madrat
#' @import magclass

calcSlowDecay <- function(landtype="crop") {

  landtype2irrigation <- c(crop   = "mixedirrig",
                           natveg = "rainfed")

  landtype2tillage    <- c(crop   = "mixedtill",
                           natveg = "notill")

  param            <- readSource("IPCCSoil", convert=FALSE)
  param.k_sopt     <- setYears(param[,,"kfacs"], NULL) # decay rate under optimum condition for slow (k4)
  cell.w_Factor    <- calcOutput("WaterEffectDecomposition", irrigation=landtype2irrigation[landtype], aggregate = FALSE)
  cell.t_Factor    <- calcOutput("TempEffectDecomposition", aggregate = FALSE)
  cell.till_Factor <- calcOutput("TillageEffectDecomposition", tillage=landtype2tillage[landtype], aggregate = FALSE)

  cell.k_slow  <- param.k_sopt * cell.w_Factor * cell.t_Factor * cell.till_Factor

  return(list(
    x=cell.k_slow,
    weight=NULL,
    unit="per yr",
    description="Decay rate for slow SOC sub-pool per year",
    isocountries=FALSE))
}
