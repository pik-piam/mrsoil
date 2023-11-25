#' @title calcSoilCarbon
#' @description Calculates SOC states
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#' @param output "actualstate" (default), "carbontransfer", "interstate", "naturalstate"
#' @param lpjml       Switch between LPJmL natveg versionstop
#' @param climatetype Switch between different climate scenarios
#'
#' @examples
#' \dontrun{
#' calcOutput("SoilCarbon")
#' }
#' @importFrom magclass setNames
#' @importFrom magpiesets findset

calcSoilCarbon <- function(output      = "actualstate",
                           lpjml       = "LPJmL4_for_MAgPIE_44ac93de",
                           climatetype = "GSWP3-W5E5:historical") {
  #######################
  ### Load Data & Ini ###
  #######################

  # Load Landuse data
  landuse               <- calcOutput("Landuse", aggregate = FALSE)
  soilCarbonSteadyState <- calcOutput("SteadyState", lpjml = lpjml, climatetype = climatetype, aggregate = FALSE)
  decay                 <- calcOutput("DecayRaw",    lpjml = lpjml, climatetype = climatetype, aggregate = FALSE)
  soilCarbonInit        <- calcOutput("SoilCarbonSpinup", aggregate = FALSE)

  ######################
  ### Carbon Cycling ###
  ######################

  out <- toolSoilCarbonCycling(soilCarbonInit,
                               soilCarbonSteadyState,
                               decay,
                               landuse)[, , output]

  return(list(x            = out,
              weight       = NULL,
              unit         = "Mt C",
              description  = "Carbon budget on croplands for historical period",
              isocountries = FALSE)
  )
}
