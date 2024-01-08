#' @title calcSoilCarbon
#' @description Calculates SOC states
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#' @param output "actualstate" (default), "carbontransfer", "interstate", "naturalstate"
#' @param lpjmlNatveg Switch between LPJmL natveg versionstop
#' @param climatetype Switch between different climate scenarios
#'
#' @examples
#' \dontrun{
#' calcOutput("SoilCarbon")
#' }
#' @importFrom magclass setNames
#' @importFrom magpiesets findset

calcSoilCarbon <- function(output      = "actualstate",
                           lpjmlNatveg = "LPJmL4_for_MAgPIE_44ac93de",
                           climatetype = "GSWP3-W5E5:historical") {
  #######################
  ### Load Data & Ini ###
  #######################

  # Load Landuse data
  landuse               <- calcOutput("Landuse", aggregate = FALSE)
  soilCarbonSteadyState <- calcOutput("SteadyState", lpjmlNatveg = lpjmlNatveg,
                                      climatetype = climatetype, aggregate = FALSE)
  decay                 <- calcOutput("DecayRaw",    lpjmlNatveg = lpjmlNatveg,
                                      climatetype = climatetype, aggregate = FALSE)
  soilCarbonInit        <- calcOutput("SoilCarbonSpinup", aggregate = FALSE)

  ######################
  ### Carbon Cycling ###
  ######################

  out <- toolSoilCarbonCycling(soilCarbonInit,
                               soilCarbonSteadyState,
                               decay,
                               landuse)[, , output]

  getSets(out, fulldim = FALSE)[1] <- "x.y.iso"

  return(list(x            = out,
              weight       = NULL,
              unit         = "Mt C",
              description  = "Carbon budget on croplands for historical period",
              isocountries = FALSE)
  )
}
