#' @title calcSoilCarbonSpinup
#' @description Calculates SOC states
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#' @param lpjmlNatveg Switch between LPJmL natveg versionstop
#' @param climatetype Switch between different climate scenarios
#'
#' @examples
#' \dontrun{
#' calcOutput("SoilCarbon")
#' }
#' @importFrom magclass setNames collapseDim
#' @importFrom magpiesets findset

calcSoilCarbonSpinup <- function(lpjmlNatveg = "LPJmL4_for_MAgPIE_44ac93de",
                                 climatetype = "GSWP3-W5E5:historical") {

  spinupStart         <- 1510
  recyclingLength     <- 30
  modelStart          <- 1901
  recyclingYears      <- c(modelStart:(modelStart + recyclingLength - 1))

  # Load steady states and decay
  soilCarbonSteadyState <- calcOutput("SteadyState", lpjmlNatveg = lpjmlNatveg, climatetype = climatetype,
                                      years = recyclingYears, aggregate = FALSE)
  decay                 <- calcOutput("DecayRaw",    lpjmlNatveg = lpjmlNatveg, climatetype = climatetype,
                                      mode = "historicalSpinup",
                                      years = recyclingYears, aggregate = FALSE)

  names                 <- as.vector(outer(c("actualstate", "naturalstate"),
                                           getItems(decay, dim = 3),
                                           paste, sep = "."))
  soilCarbonInit        <- new.magpie(getItems(decay, dim = 1), spinupStart, names)
  soilCarbonInit[, ,  "actualstate"][, , "crop"]   <- soilCarbonSteadyState[, 1, "natveg"]
  soilCarbonInit[, ,  "actualstate"][, , "natveg"] <- soilCarbonSteadyState[, 1, "natveg"]
  soilCarbonInit[, , "naturalstate"][, , "crop"]   <- soilCarbonSteadyState[, 1, "natveg"]
  soilCarbonInit[, , "naturalstate"][, , "natveg"] <- soilCarbonSteadyState[, 1, "natveg"]

  for (i in 1:((modelStart - 1 - spinupStart) / recyclingLength)) {

    tmpYears    <- (spinupStart + 1):(spinupStart + recyclingLength) + (i - 1) * recyclingLength

    # Load Landuse data for spinup period
    period  <- paste0("states_", tmpYears[1] - 1, "to", tmpYears[recyclingLength])
    landuse <- calcOutput("Landuse", period = period, aggregate = FALSE)
    tmp     <- toolSoilCarbonCycling(soilCarbonInit,
                                     setYears(soilCarbonSteadyState, tmpYears),
                                     setYears(decay,                 tmpYears),
                                     landuse)
    soilCarbonInit <- tmp[, tail(tmpYears, 1), c("actualstate", "naturalstate")]
  }

  getSets(soilCarbonInit, fulldim = FALSE)[1] <- "x.y.iso"

  return(list(x            = soilCarbonInit,
              weight       = NULL,
              unit         = "Mt C",
              min          = 0,
              description  = "Soil carbon stocks on croplands for starting of the modelling period",
              isocountries = FALSE)
  )
}
