#' @title calcDecay
#' @description This function wraps together the decay rate for allSOC sub-pool per year for mineral soils
#' using the steady-state method (Tier 2) of the 2019 Refinement to the 2006 IPP Guidelines
#' for National Greenhouse Gas Inventories
#' @param tillage tillage type to de considered.
#'                   Default (histill) is historic tillage area shares based on no tillage areas from Porwollik together with rule based assumption;
#'                   'mixedtill' includes pure rule based assumptions.
#' @param climate_scen climate configuration
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#' @examples
#' \dontrun{ calcOutput("Decay", aggregate = FALSE) }
#'
#' @import madrat
#' @import magclass

calcDecay <- function(tillage="histtill", climate_scen="default") {

  if(is.null(tillage)) tillage <- "histtill"

  param <- readSource("IPCCSoil", convert=FALSE)
  # kfaca = decay rate under optimum condition for active (k3)
  # kfacs = decay rate under optimum condition for slow (k4)
  # kfacp = decay rate under optimum condition for passive (k5)
  # k3par1 = sand intercept
  # k3par2 = sand slope

  cell.w_Factor    <- mbind(setNames(calcOutput("WaterEffectDecomposition", climate_scen=climate_scen, irrigation = "mixedirrig", aggregate = FALSE),"crop"),
                            setNames(calcOutput("WaterEffectDecomposition", climate_scen=climate_scen, irrigation = "rainfed", aggregate = FALSE),"natveg"))
  cell.till_Factor <- setNames(calcOutput("TillageEffectDecomposition", tillage = tillage, aggregate = FALSE)[,,rep(1,2)],c("crop","natveg"))
  cell.till_Factor[,,"natveg"] <- calcOutput("TillageEffectDecomposition", tillage = "notill", aggregate = FALSE)[,,]

  cell.t_Factor    <- calcOutput("TempEffectDecomposition", climate_scen=climate_scen, aggregate = FALSE)
  cell.sand_frac   <- calcOutput("SandFrac", aggregate = FALSE)

  ActiveDecay    <-  param[,,"kfaca"] * cell.w_Factor * cell.t_Factor * cell.till_Factor *
                      ( param[,,"k3par1"] +  param[,,"k3par2"] * cell.sand_frac )
  SlowDecay      <- param[,,"kfacs"] * cell.w_Factor * cell.t_Factor * cell.till_Factor
  PassiveDecay   <- param[,,"kfacp"] * cell.w_Factor * cell.t_Factor

  .clean <- function(x,name) return(add_dimension(collapseNames(x),dim=3.2,add="subpool",nm=name))

  decay <- magpiesort(mbind(.clean(ActiveDecay,"active"),
                            .clean(SlowDecay,"slow"),
                            .clean(PassiveDecay,"passive")))

  return(list(
    x=decay,
    weight=NULL,
    unit="per yr",
    description="Decay rate for all SOC sub-pool per year",
    isocountries=FALSE))
}
