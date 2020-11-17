#' @title calcActiveDecay
#' @description This function calculates the decay rate for active SOC sub-pool per year for mineral soils
#' using the steady-state method (Tier 2) of the 2019 Refinement to the 2006 IPP Guidelines
#' for National Greenhouse Gas Inventories
#'
#' @param tillage tillage type to de considered.
#'                   Default (histill) is historic tillage area shares based on no tillage areas from Porwollik together with rule based assumption;
#'                   'mixedtill' includes pure rule based assumptions. Other options: fulltill, notill, reducedtill
#' @return magpie object in cellular resolution
#' @author Kristine Karstens, Jan Philipp Dietrich
#'
#' @examples
#' \dontrun{ calcOutput("ActiveDecay", aggregate = FALSE) }
#'
#' @import madrat
#' @import magclass

calcActiveDecay <- function(tillage="histill"){

  param                <- readSource("IPCCSoil", convert=FALSE)
  param.k_aopt         <- param[,,"kfaca"] # decay rate under optimum condition for active (k3)
  param.sand_intercept <- param[,,"k3par1"]
  param.sand_slope     <- param[,,"k3par2"]

  cell.w_Factor    <- mbind(setNames(calcOutput("WaterEffectDecomposition", irrigation = "mixedirrig", aggregate = FALSE),"crop"),
                            setNames(calcOutput("WaterEffectDecomposition", irrigation = "rainfed", aggregate = FALSE),"natveg"))
  cell.till_Factor <- mbind(setNames(calcOutput("TillageEffectDecomposition", tillage = tillage, aggregate = FALSE),"crop"),
                            setNames(calcOutput("TillageEffectDecomposition", tillage = "notill", aggregate = FALSE),"natveg"))

  cell.t_Factor    <- calcOutput("TempEffectDecomposition", aggregate = FALSE)
  cell.sand_frac   <- calcOutput("SandFrac", aggregate = FALSE)

  cell.k_active    <-  param.k_aopt * cell.w_Factor * cell.t_Factor * cell.till_Factor *
                       ( param.sand_intercept +  param.sand_slope * cell.sand_frac )

  cell.k_active    <- collapseNames(cell.k_active[,sort(getItems(cell.k_active, dim=2)),])

  return(list(
    x=cell.k_active,
    weight=NULL,
    unit="per yr",
    description="Decay rate for active SOC sub-pool per year",
    isocountries=FALSE))
}

