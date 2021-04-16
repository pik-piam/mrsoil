#' @title calcSoilCarbon
#' @description Calculates SOC states
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#' @param init 'lu' for land-use specific initialisation, 'natveg' initialisation with natural vegetation stocks
#' @param output 'full' - all important soil related values, 'reduced' - just SOC state values
#' @param cfg run configuration
#' @param cfg_default run configuration for default setup (used for spinup setup)
#' @examples
#' \dontrun{
#' calcOutput("SoilCarbon")
#' }
#' @importFrom magclass setNames
#' @importFrom magpiesets findset

calcSoilCarbon <- function(init="spinup", output="full", cfg=NULL, cfg_default=NULL){

  model_start <- 1901
  years       <- seq(model_start, 2010, 1)

  #######################
  ### Load Data & Ini ###
  #######################

  # Load Landuse data
  Landuse            <- calcOutput("Landuse",       landuse_scen=cfg$landuse, aggregate=FALSE)[,years,]
  LanduseChange      <- calcOutput("LanduseChange", landuse_scen=cfg$landuse, aggregate=FALSE)[,years[-1],]

  # Load steady states (setting to zero for all non cropland cells)
  SoilCarbonSteadyState <- calcOutput("SteadyState", cfg=cfg, aggregate = FALSE)[,years,]
  noCropCells           <- which(Landuse[,,"crop"]==0)
  for(sub in getNames(SoilCarbonSteadyState, dim=2)){
    SoilCarbonSteadyState[,,"crop"][,,sub][noCropCells] <- 0  #Clear cells with no Cropland
  }

  # Loading decay rates (cutting over 1)
  Decay                 <- calcOutput("Decay", tillage=cfg$tillage, climate_scen=cfg$climate, aggregate = FALSE)
  Decay[Decay>1]        <- 1


  SoilCarbon            <- SoilCarbonSteadyState
  SoilCarbon[]          <- 0

  # Initialize SOC stocks
  if(init=="spinup"){     SoilCarbon[,years[1],] <- calcOutput("SoilCarbonSpinup", model_start=model_start, cfg_default=cfg_default, aggregate=FALSE)
  } else if(init=="lu"){  SoilCarbon[,years[1],] <- SoilCarbonSteadyState[,years[1],]
  }

  ######################
  ### Carbon Cycling ###
  ######################

  out <- toolSoilCarbonCycling(SoilCarbon,
                               SoilCarbonSteadyState,
                               Decay,
                               Landuse,
                               LanduseChange)

  if(output == "reduced") out <- out[,,"actualstate"]

  return(list( x            = out,
               weight       = NULL,
               unit         = "Mt C",
               description  = "Carbon budget on croplands for historical period",
               isocountries = FALSE)
    )
}

