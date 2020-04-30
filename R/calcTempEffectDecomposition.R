#' @title calcTempEffectDecomposition
#' @description This function calculates the temperature effect on decomposition for mineral soils
#' using the steady-state method (Tier 2) of the 2019 Refinement to the 2006 IPP Guidelines
#' for National Greenhouse Gas Inventories
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#' @examples
#' \dontrun{ calcOutput("TempEffectDecomposition", aggregate = FALSE) }
#'
#' @import madrat
#' @import magclass
#' @importFrom magpiesets findset

calcTempEffectDecomposition <- function() {

  param <- readSource("IPCCSoil", convert=FALSE)
  param.t_max <- setYears(param[,,"tmax"],NULL)
  param.t_opt <- setYears(param[,,"topt"],NULL)
  param.t_a   <- setYears(param[,,"ta"],NULL)
  param.t_b   <- setYears(param[,,"tb"],NULL)
  cell.temp   <- readSource("CRU", subtype="temperature", convert = "onlycorrect")[,sort(findset("past_all")),]

  tempFunc            <- function(x){ x**param.t_a * exp(0.076 * (1 - x**param.t_b )) }
  tempArg             <- (param.t_max - cell.temp)/(param.t_max - param.t_opt)

  cell.t_monthFactor  <- collapseNames(tempFunc(tempArg))
  cell.t_monthFactor  <- ifelse(cell.temp > 45, 0, cell.t_monthFactor)
  cell.t_Factor       <- dimSums(cell.t_monthFactor, dim=3)/12

  return(list(
    x=cell.t_Factor,
    weight=NULL,
    unit="",
    description="Temperature effect on decomposition for mineral soils (unitless)",
    isocountries=FALSE))
}
