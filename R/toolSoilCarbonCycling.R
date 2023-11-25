#' @title toolSoilCarbonCycling
#' @description This function cycles the carbon on an annual basis between the different soil pools
#'
#' @param soilCarbonInit soil carbon initialization
#' @param soilCarbonSteadyState steadystates
#' @param decay decay rates
#' @param landuse landuse
#'
#' @return magpie object with global parameters
#' @author Kristine Karstens
#'
#' @export

toolSoilCarbonCycling <- function(soilCarbonInit, soilCarbonSteadyState, decay, landuse) {

  years         <- getYears(soilCarbonSteadyState, as.integer = TRUE)
  landuseChange <- toolLanduseChange(landuse)

  # correct SteadyStates for bo cropland -> no cropland soil
  noCropCells           <- which(landuse[, years, "crop"] == 0)
  for (sub in getNames(soilCarbonSteadyState, dim = 2)) {
    soilCarbonSteadyState[, , "crop"][, , sub][noCropCells] <- 0  # Clear cells with no Cropland
  }

  # clear cells with no land use -> no soil
  noSoilCells               <- where(dimSums(landuse[, 1, ], dim = 3) == 0)$true$regions
  soilCarbonSteadyState[noSoilCells, , ] <- 0

  # cut decay rates above 1
  decay[decay > 1]        <- 1

  # Initialize outputs
  soilCarbon             <- mbind(collapseDim(soilCarbonInit[, , "actualstate"],  dim = 3.1), soilCarbonSteadyState)
  soilCarbonNatural      <- mbind(collapseDim(soilCarbonInit[, , "naturalstate"], dim = 3.1), soilCarbonSteadyState)
  soilCarbon[, years, ]    <- 0
  soilCarbonTransfer     <- soilCarbonInter   <- soilCarbon
  soilCarbonTransfer[]   <- soilCarbonInter[] <- 0

  for (t in years) {
    # Calculate carbon transfer between landuse types
    soilCarbonTransfer[, t, ] <-
      setYears(mbind(add_dimension(collapseNames(soilCarbon[, t - 1, "crop"]),  nm = "natveg"),
                     add_dimension(collapseNames(soilCarbon[, t - 1, "natveg"]), nm = "crop")), t) *
      landuseChange[, t, "expansion"] - setYears(soilCarbon[, t - 1, ], t) * landuseChange[, t, "reduction"]

    # Calculate the carbon density after landuse change
    soilCarbonInter[, t, ]    <- (setYears(soilCarbon[, t - 1, ], t) * setYears(landuse[, t - 1, ], t)  +
                                    soilCarbonTransfer[, t, ]) / landuse[, t, ]
    soilCarbonInter[, t, ]    <- toolConditionalReplace(soilCarbonInter[, t, ],
                                                        conditions = c("is.na()", "is.infinite()"),
                                                        replaceby = 0)

    # Update the carbon density after input and decay
    soilCarbon[, t, ]         <- (soilCarbonInter[, t, ] +
                                    (soilCarbonSteadyState[, t, ] - soilCarbonInter[, t, ]) * decay[, t, ])

    # Calculate counterfactual potential natural vegetation stocks
    soilCarbonNatural[, t, ]  <- (setYears(soilCarbonNatural[, t - 1, ], t) +
                                    (soilCarbonSteadyState[, t, ] -
                                       setYears(soilCarbonNatural[, t - 1, ], t)) * decay[, t, ])
    soilCarbonNatural[, , "crop"]  <- 0

    print(t)
  }
  out <- mbind(add_dimension(soilCarbon,            dim = 3.1, add = "var", nm = "actualstate"),
               add_dimension(soilCarbonTransfer,    dim = 3.1, add = "var", nm = "carbontransfer"),
               add_dimension(soilCarbonInter,       dim = 3.1, add = "var", nm = "interstate"),
               add_dimension(soilCarbonNatural,     dim = 3.1, add = "var", nm = "naturalstate"))

  return(out)
}
