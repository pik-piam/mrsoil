#' @title calcSoilCarbonSpinup
#' @description Calculates SOC states
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#' @param cfg_default run configuration for default setup (used for spinup setup)
#' @param model_start start year
#' @examples
#' \dontrun{
#' calcOutput("SoilCarbon")
#' }
#' @importFrom magclass setNames
#' @importFrom magpiesets findset

calcSoilCarbonSpinup <- function(model_start=1901, cfg_default=NULL){

  recycling_length     <- 30
  spinup_start         <- 1800

  if((model_start-spinup_start)>150) warning("The time period is very long. Please consider splitting it up in parts.")

  # Load Landuse dta for spinup period
  spinup             <- paste0("spinup_", spinup_start, "to", model_start)
  Landuse            <- calcOutput("Landuse",       landuse_scen=spinup, aggregate=FALSE)
  LanduseChange      <- calcOutput("LanduseChange", landuse_scen=spinup, aggregate=FALSE)


  # Load steady states (setting to zero for all non cropland cells)
  SoilCarbonSteadyState <- calcOutput("SteadyState", cfg=cfg_default, aggregate = FALSE)
  noCropCells           <- which(Landuse[,,"crop"]==0)
  for(sub in getNames(SoilCarbonSteadyState, dim=2)){
    SoilCarbonSteadyState[,,"crop"][,,sub][noCropCells] <- 0  #Clear cells with no Cropland
  }

  # Loading decay rates (cutting over 1)
  Decay                 <- calcOutput("Decay", tillage=cfg_default$tillage, climate_scen=cfg_default$climate, aggregate = FALSE)
  Decay[Decay>1]        <- 1

  # Recycle management and climate data (in form of Decay and SteadyState stocks) for recycling period
  spinup_years    <- c(spinup_start:model_start)
  recycling_years <- c(model_start:(model_start+recycling_length-1))
  spinup_length   <- (model_start - spinup_start)
  spinup_recycled <- tail(c(rep(recycling_years, (spinup_length%/%recycling_length+1)), model_start), spinup_length+1)

  SoilCarbonSteadyState <- setYears(SoilCarbonSteadyState[,spinup_recycled,], spinup_years)
  Decay                 <- setYears(Decay[,spinup_recycled,],                 spinup_years)
  SoilCarbon            <- SoilCarbonSteadyState
  SoilCarbon[]          <- 0

  ### consider overwriting the years o not have so many big objects at the same time
  out <- toolSoilCarbonCycling(SoilCarbon,
                               SoilCarbonSteadyState,
                               Decay,
                               Landuse,
                               LanduseChange)

  out <- out[,model_start,"actualstate"]

  return(list( x            = out,
               weight       = NULL,
               unit         = "Mt C",
               description  = "Soil carbon stocks on croplands for starting of the modelling period",
               isocountries = FALSE)
  )
}

