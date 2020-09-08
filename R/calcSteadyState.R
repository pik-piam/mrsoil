#' @title calcSteadyState
#' @description This function wraps together the steady state for all sub-pool SOC stock for mineral soils
#' using the steady-state method (Tier 2) of the 2019 Refinement to the 2006 IPP Guidelines
#' for National Greenhouse Gas Inventories
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#' @examples
#' \dontrun{ calcOutput("SteadyState", aggregate = FALSE) }
#'
#' @import madrat
#' @import magclass

calcSteadyState <- function() {

  ActiveSteadyState  <- mbind(setNames(calcOutput("ActiveSteadyState",  landtype="crop",   aggregate = FALSE),"crop"),
                              setNames(calcOutput("ActiveSteadyState",  landtype="natveg", aggregate = FALSE),"natveg"))

  SlowSteadyState    <- mbind(setNames(calcOutput("SlowSteadyState",  landtype="crop",   aggregate = FALSE),"crop"),
                              setNames(calcOutput("SlowSteadyState",  landtype="natveg", aggregate = FALSE),"natveg"))

  PassiveSteadyState <- mbind(setNames(calcOutput("PassiveSteadyState",  landtype="crop",   aggregate = FALSE),"crop"),
                              setNames(calcOutput("PassiveSteadyState",  landtype="natveg", aggregate = FALSE),"natveg"))

  subpools              <- c("active","slow","passive")
  names                 <- as.vector(outer(getNames(ActiveSteadyState), subpools, paste, sep="."))
  SoilCarbonSteadyState <- new.magpie(getCells(ActiveSteadyState), getYears(ActiveSteadyState), names, fill = 0)

  SoilCarbonSteadyState[,,"active"]  <- ActiveSteadyState
  SoilCarbonSteadyState[,,"slow"]    <- SlowSteadyState
  SoilCarbonSteadyState[,,"passive"] <- PassiveSteadyState

  return(list(
    x=SoilCarbonSteadyState,
    weight=NULL,
    unit="per yr",
    description="Decay rate for all SOC sub-pool per year",
    isocountries=FALSE))
}
