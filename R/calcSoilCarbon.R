#' @title calcSoilCarbon
#' @description Calculates SOC states
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#' @param init 'lu' for land-use specific initialisation, 'natveg' initialisation with natural vegetation stocks
#' @param output 'full' - all important soil related values, 'reduced' - just SOC state values
#' @examples
#' \dontrun{
#' calcOutput("SoilCarbon")
#' }
#' @importFrom magclass setNames
#' @importFrom magpiesets findset

calcSoilCarbon <- function(init="lu", output="full"){

  years <- sort(as.numeric(substring(findset("past_all"),2)))

  #######################
  ### Load Data & Ini ###
  #######################

  # Load Landuse data
  Landuse            <- calcOutput("Landuse",       aggregate=FALSE)
  LanduseChange      <- calcOutput("LanduseChange", aggregate=FALSE)

  # Load steady states (setting to zero for all non cropland cells)
  SoilCarbonSteadyState <- calcOutput("SteadyState", aggregate = FALSE)
  noCropCells           <- which(Landuse[,,"crop"]==0)
  for(sub in getNames(SoilCarbonSteadyState, dim=2)){
    SoilCarbonSteadyState[,,sub][noCropCells] <- 0  #Clear cells with no Cropland
  }

  # Loading decay rates (cutting over 1)
  Decay                 <- calcOutput("Decay", aggregate = FALSE)
  Decay[Decay>1]        <- 1

  SoilCarbon            <- SoilCarbonSteadyState
  SoilCarbon[]          <- 0

  # Initialize SOC stocks
  if(init=="natveg"){
    SoilCarbon[,years[1],]     <- mbind(         SoilCarbonSteadyState[,years[1],"natveg"],
                                        setNames(SoilCarbonSteadyState[,years[1],"natveg"],
                                                 paste0("crop.",getNames(SoilCarbonSteadyState[,years[1],"natveg"], dim=2))))

  } else if(init=="lu"){
    SoilCarbon[,years[1],]     <- SoilCarbonSteadyState[,years[1],]

  } else if(init=="mixed"){

    SoilCarbon[,years[1],]     <- mbind(         SoilCarbonSteadyState[,years[1],"natveg"],
                                                 setNames(SoilCarbonSteadyState[,years[1],"natveg"],
                                                          paste0("crop.",getNames(SoilCarbonSteadyState[,years[1],"natveg"], dim=2))))

    SoilCarbon[,years[1],]     <- (SoilCarbonSteadyState[,years[1],] + SoilCarbon[,years[1],])/2

  }

  #Clear cells with no Landuse -> no Soil
  noSoilCells               <- where(dimSums(Landuse, dim=3)==0)$true$regions
  SoilCarbon[noSoilCells,,] <- 0
  SoilCarbonSteadyState[noSoilCells,,] <- 0

  #Initialize outputs
  SoilCarbonTransfer      <- SoilCarbonInter        <- SoilCarbonNatural    <- SoilCarbon
  SoilCarbonTransfer[,1,] <- SoilCarbonInter[,1,]   <- 0

  ######################
  ### Looping        ###
  ######################

  for(year_x in years[years!=years[1]]){

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

  if(output=="reduced"){

    out <- SoilCarbon

  } else if(output=="full"){

    out <- mbind(add_dimension(SoilCarbon,            dim=3.1, add="var", nm="actualstate"),
                 add_dimension(SoilCarbonTransfer,    dim=3.1, add="var", nm="carbontransfer"),
                 add_dimension(SoilCarbonInter,       dim=3.1, add="var", nm="interstate"),
                 add_dimension(SoilCarbonNatural,     dim=3.1, add="var", nm="naturalstate"))
  }

  return(list( x            = out,
               weight       = NULL,
               unit         = "Mt C",
               description  = "Carbon budget on croplands for historical period",
               isocountries = FALSE)
    )
}

