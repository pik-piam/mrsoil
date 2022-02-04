#' @title calcSoilCarbonSpinup
#' @description Calculates SOC states
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#' @param cfg_default run configuration for default setup (used for spinup setup)
#' @param spinup_start start year of spinup (model_start-1-spinup_start has to be a multiple of recycling length)
#' @examples
#' \dontrun{
#' calcOutput("SoilCarbon")
#' }
#' @importFrom magclass setNames collapseDim
#' @importFrom magpiesets findset

calcSoilCarbonSpinup <- function(spinup_start=1510, cfg_default=NULL){

  recycling_length     <- 30
  model_start          <- 1901
  recycling_years      <- c(model_start:(model_start+recycling_length-1))

  # Load steady states and decay
  SoilCarbonSteadyState <- calcOutput("SteadyState", cfg=cfg_default, years=recycling_years, aggregate = FALSE)
  Decay                 <- calcOutput("Decay", tillage=cfg_default$tillage, climate_scen=cfg_default$climate,
                                      years=recycling_years, aggregate = FALSE)

  names                 <- as.vector(outer(c("actualstate", "naturalstate"),
                                           getItems(Decay, dim = 3),
                                           paste, sep = "."))
  SoilCarbonInit        <- new.magpie(getItems(Decay, dim = 1), spinup_start, names)
  SoilCarbonInit[, , "actualstate.crop"]    <- SoilCarbonSteadyState[, 1, "natveg"]
  SoilCarbonInit[, , "actualstate.natveg"]  <- SoilCarbonSteadyState[, 1, "natveg"]
  SoilCarbonInit[, , "naturalstate.crop"]   <- SoilCarbonSteadyState[, 1, "natveg"]
  SoilCarbonInit[, , "naturalstate.natveg"] <- SoilCarbonSteadyState[, 1, "natveg"]

  for(i in 1:((model_start-1-spinup_start)/recycling_length)){

    tmp_years    <- (spinup_start+1):(spinup_start+recycling_length)+(i-1)*recycling_length

    # Load Landuse dta for spinup period
    scen    <- paste0("spinup_", tmp_years[1]-1, "to", tmp_years[recycling_length])
    Landuse <- calcOutput("Landuse", landuse_scen=scen, aggregate=FALSE)
    tmp     <- toolSoilCarbonCycling(SoilCarbonInit,
                                     setYears(SoilCarbonSteadyState, tmp_years),
                                     setYears(Decay,                 tmp_years),
                                     Landuse)
    SoilCarbonInit <- tmp[, tail(tmp_years, 1), c("actualstate", "naturalstate")]
  }

  return(list( x            = SoilCarbonInit,
               weight       = NULL,
               unit         = "Mt C",
               min          = 0,
               description  = "Soil carbon stocks on croplands for starting of the modelling period",
               isocountries = FALSE)
  )
}

