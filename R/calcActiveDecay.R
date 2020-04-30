#' @title calcActiveDecay
#' @description This function calculates the decay rate for active SOC sub-pool per year for mineral soils
#' using the steady-state method (Tier 2) of the 2019 Refinement to the 2006 IPP Guidelines
#' for National Greenhouse Gas Inventories
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#' @param landtype 'crop' for cropland, 'natveg' for all the rest
#' @examples
#' \dontrun{ calcOutput("ActiveDecay", aggregate = FALSE) }
#'
#' @import madrat
#' @import magclass

calcActiveDecay <- function(landtype="crop"){

  landtype2irrigation <- c(crop   = "mixedirrig",
                           natveg = "rainfed")

  landtype2tillage    <- c(crop   = "mixedtill",
                           natveg = "notill")

  param                <- readSource("IPCCSoil", convert=FALSE)
  param.k_aopt         <- setYears(param[,,"kfaca"], NULL) # decay rate under optimum condition for active (k3)
  param.sand_intercept <- setYears(param[,,"k3par1"], NULL)
  param.sand_slope     <- setYears(param[,,"k3par2"], NULL)

  cell.w_Factor    <- calcOutput("WaterEffectDecomposition", irrigation = landtype2irrigation[landtype], aggregate = FALSE)
  cell.t_Factor    <- calcOutput("TempEffectDecomposition", aggregate = FALSE)
  cell.till_Factor <- calcOutput("TillageEffectDecomposition", tillage = landtype2tillage[landtype], aggregate = FALSE)
  cell.sand_frac   <- calcOutput("SandFrac", aggregate = FALSE)

  cell.k_active    <-  param.k_aopt * cell.w_Factor * cell.t_Factor * cell.till_Factor *
                       ( param.sand_intercept +  param.sand_slope * cell.sand_frac )

  #cell.k_active   <- collapseNames(cell.k_active)

  return(list(
    x=cell.k_active,
    weight=NULL,
    unit="per yr",
    description="Decay rate for active SOC sub-pool per year",
    isocountries=FALSE))
}

