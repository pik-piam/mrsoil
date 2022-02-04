#' @title toolSoilCarbonCycling
#' @description This function cycles the carbon on an annual basis between the different soil pools
#'
#' @param SoilCarbonInit soil carbon initialization
#' @param SoilCarbonSteadyState steadystates
#' @param Decay decay rates
#' @param Landuse landuse
#'
#' @return magpie object with global parameters
#' @author Kristine Karstens
#'
#' @export

toolSoilCarbonCycling <- function(SoilCarbonInit, SoilCarbonSteadyState, Decay, Landuse) {

  years         <- getYears(SoilCarbonSteadyState, as.integer = TRUE)
  LanduseChange <- toolLanduseChange(Landuse)

  # correct SteadyStates for bo cropland -> no cropland soil
  noCropCells           <- which(Landuse[,years,"crop"]==0)
  for(sub in getNames(SoilCarbonSteadyState, dim=2)){
    SoilCarbonSteadyState[,,"crop"][,,sub][noCropCells] <- 0  #Clear cells with no Cropland
  }

  # clear cells with no land use -> no Soil
  noSoilCells               <- where(dimSums(Landuse[,1,], dim=3)==0)$true$regions
  SoilCarbonSteadyState[noSoilCells,,] <- 0

  # cut decay rates above 1
  Decay[Decay>1]        <- 1

  #Initialize outputs
  SoilCarbon             <- mbind(collapseDim(SoilCarbonInit[, , "actualstate"]), SoilCarbonSteadyState)
  SoilCarbonNatural      <- mbind(collapseDim(SoilCarbonInit[, , "naturalstate"]), SoilCarbonSteadyState)
  SoilCarbon[,years,]    <- 0
  SoilCarbonTransfer     <- SoilCarbonInter   <- SoilCarbon
  SoilCarbonTransfer[]   <- SoilCarbonInter[] <- 0

  for(year_x in years){

    # Calculate carbon transfer between landuse types
    SoilCarbonTransfer[,year_x,] <- (setYears(mbind(add_dimension(collapseNames(SoilCarbon[,year_x-1 ,"crop"]),  nm="natveg"),
                                                    add_dimension(collapseNames(SoilCarbon[,year_x-1,"natveg"]), nm="crop")), year_x) *
                                       LanduseChange[,year_x,"expansion"]
                                     - setYears(SoilCarbon[,year_x-1,], year_x) * LanduseChange[,year_x,"reduction"])

    # Calculate the carbon density after landuse change
    SoilCarbonInter[,year_x,]    <- (setYears(SoilCarbon[,year_x-1,], year_x) * setYears(Landuse[,year_x-1,], year_x)
                                     + SoilCarbonTransfer[,year_x,] ) / Landuse[,year_x,]
    SoilCarbonInter[,year_x,]    <- toolConditionalReplace(SoilCarbonInter[,year_x,], conditions = c("is.na()","is.infinite()"), replaceby = 0)

    # Update the carbon density after input and decay
    SoilCarbon[,year_x,]         <- SoilCarbonInter[,year_x,] + (SoilCarbonSteadyState[,year_x,] - SoilCarbonInter[,year_x,]) * Decay[,year_x,]

    # Calculate counterfactual potential natural vegetation stocks
    SoilCarbonNatural[,year_x,]  <- setYears(SoilCarbonNatural[,year_x-1,], year_x) + (SoilCarbonSteadyState[,year_x,] - setYears(SoilCarbonNatural[,year_x-1,], year_x)) * Decay[,year_x,]
    SoilCarbonNatural[,,"crop"]  <- 0

    print(year_x)
  }

  out <- mbind(add_dimension(SoilCarbon,            dim=3.1, add="var", nm="actualstate"),
               add_dimension(SoilCarbonTransfer,    dim=3.1, add="var", nm="carbontransfer"),
               add_dimension(SoilCarbonInter,       dim=3.1, add="var", nm="interstate"),
               add_dimension(SoilCarbonNatural,     dim=3.1, add="var", nm="naturalstate"))

  return(out)
}
