#' @title calcDecay
#' @description This function wraps together the decay rate for allSOC sub-pool per year for mineral soils
#' using the steady-state method (Tier 2) of the 2019 Refinement to the 2006 IPP Guidelines
#' for National Greenhouse Gas Inventories
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#' @examples
#' \dontrun{ calcOutput("Decay", aggregate = FALSE) }
#'
#' @import madrat
#' @import magclass

calcDecay <- function() {

  ActiveDecay         <- mbind(setNames(calcOutput("ActiveDecay",  landtype="crop",   aggregate = FALSE),"crop"),
                               setNames(calcOutput("ActiveDecay",  landtype="natveg", aggregate = FALSE),"natveg"))

  SlowDecay           <- mbind(setNames(calcOutput("SlowDecay",  landtype="crop",   aggregate = FALSE),"crop"),
                               setNames(calcOutput("SlowDecay",  landtype="natveg", aggregate = FALSE),"natveg"))

  PassiveDecay        <- mbind(setNames(calcOutput("PassiveDecay", landtype="crop",   aggregate = FALSE),"crop"),
                               setNames(calcOutput("PassiveDecay", landtype="natveg", aggregate = FALSE),"natveg"))

  subpools            <- c("active","slow","passive")
  names               <- as.vector(outer(getNames(ActiveDecay), subpools, paste, sep="."))
  Decay               <- new.magpie(getCells(ActiveDecay), getYears(ActiveDecay), names, fill = 0)

  Decay[,,"active"]   <- ActiveDecay
  Decay[,,"slow"]     <- SlowDecay
  Decay[,,"passive"]  <- PassiveDecay


  return(list(
    x=Decay,
    weight=NULL,
    unit="per yr",
    description="Decay rate for all SOC sub-pool per year",
    isocountries=FALSE))
}
