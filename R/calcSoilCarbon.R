#' @title calcSoilCarbon
#' @description Calculates SOC states
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#' @param output 'full' - all important soil related values, 'reduced' - just SOC state values
#' @param cfg run configuration
#' @param cfg_default run configuration for default setup (used for spinup setup)
#' @examples
#' \dontrun{
#' calcOutput("SoilCarbon")
#' }
#' @importFrom magclass setNames
#' @importFrom magpiesets findset

calcSoilCarbon <- function(output="full", cfg=NULL, cfg_default=NULL){

  #######################
  ### Load Data & Ini ###
  #######################

  # Load Landuse data
  Landuse               <- calcOutput("Landuse", landuse_scen=cfg$landuse, aggregate=FALSE)
  SoilCarbonSteadyState <- calcOutput("SteadyState", cfg=cfg, aggregate = FALSE)
  Decay                 <- calcOutput("Decay", tillage=cfg$tillage, climate_scen=cfg$climate, aggregate = FALSE)

  if(cfg$soilinit>1900){

    years <- getYears(Landuse, as.integer=TRUE)
    years <- years[years >= cfg$soilinit]
    SoilCarbonInit        <- mbind(setNames(setYears(SoilCarbonSteadyState[,years[1],"natveg"],years[1]-1),
                                            paste0("crop.",getNames(SoilCarbonSteadyState[,,"natveg"], dim=2))),
                                   setYears(SoilCarbonSteadyState[,years[1],"natveg"],years[1]-1))
    SoilCarbonSteadyState <- SoilCarbonSteadyState[,years,]
    Decay                 <- Decay[,years,]
    Landuse               <- Landuse[,c(years[1]-1,years),]

  } else if(cfg$soilinit<1){


    SoilCarbonInit        <- setYears(SoilCarbonSteadyState[,"y1901",],"y1900")

  } else {

    SoilCarbonInit        <- calcOutput("SoilCarbonSpinup", spinup_start=cfg$soilinit, cfg_default=cfg_default, aggregate=FALSE)
  }


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

