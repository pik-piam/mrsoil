#' @title calcSoilCarbon
#' @description Calculates SOC states
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#' @param spinup_start start of initialization with natural vegetation stocks
#' @param output 'full' - all important soil related values, 'reduced' - just SOC state values
#' @param cfg run configuration
#' @param cfg_default run configuration for default setup (used for spinup setup)
#' @examples
#' \dontrun{
#' calcOutput("SoilCarbon")
#' }
#' @importFrom magclass setNames
#' @importFrom magpiesets findset

calcSoilCarbon <- function(spinup_start="1510", output="full", cfg=NULL, cfg_default=NULL){

  #######################
  ### Load Data & Ini ###
  #######################

  # Load Landuse data
  Landuse               <- calcOutput("Landuse", landuse_scen=cfg$landuse, aggregate=FALSE)
  SoilCarbonSteadyState <- calcOutput("SteadyState", cfg=cfg, aggregate = FALSE)
  Decay                 <- calcOutput("Decay", tillage=cfg$tillage, climate_scen=cfg$climate, aggregate = FALSE)
  SoilCarbonInit        <- calcOutput("SoilCarbonSpinup", spinup_start=spinup_start, cfg_default=cfg_default, aggregate=FALSE)

  ######################
  ### Carbon Cycling ###
  ######################

  out <- toolSoilCarbonCycling(SoilCarbonInit,
                               SoilCarbonSteadyState,
                               Decay,
                               Landuse)

  if(output == "reduced") out <- out[,,"actualstate"]

  return(list( x            = out,
               weight       = NULL,
               unit         = "Mt C",
               description  = "Carbon budget on croplands for historical period",
               isocountries = FALSE)
    )
}

