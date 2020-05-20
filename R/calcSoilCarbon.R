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

  #1 calc all targets (steady states for all time steps for all sub pools)
  #2 Load Landuse, LanduseChange data
  #3 init all cell for natveg, crop and all sub pools -> use steady states in zero-th year
  #4 loop over the years
  ## * calc SoilCarbon*(t-1) after LanduseChange
  ##   = ( SoilCarbon(t-1, lu) * lu(t-1, lu)
  ##     - SoilCarbon(t-1, lu)  * lu_red(t, lu)
  ##     + SoilCarbon(t-1, !lu) * lu_exp(t, lu)) /
  ##     lu(t,lu)
  ## * update soil Carbon
  ##   = SoilCarbon*(t-1,lu) + (SoilCarbonSteadyState(t,lu) - SoilCarbon*(t-1,lu)) * Decay(t,lu)


  #######################
  ### Load Data & Ini ###
  #######################

  ActiveSteadyState  <- mbind(setNames(calcOutput("ActiveSteadyState",  landtype="crop",   aggregate = FALSE),"crop"),
                              setNames(calcOutput("ActiveSteadyState",  landtype="natveg", aggregate = FALSE),"natveg"))

  SlowSteadyState    <- mbind(setNames(calcOutput("SlowSteadyState",  landtype="crop",   aggregate = FALSE),"crop"),
                              setNames(calcOutput("SlowSteadyState",  landtype="natveg", aggregate = FALSE),"natveg"))

  PassiveSteadyState <- mbind(setNames(calcOutput("PassiveSteadyState",  landtype="crop",   aggregate = FALSE),"crop"),
                              setNames(calcOutput("PassiveSteadyState",  landtype="natveg", aggregate = FALSE),"natveg"))

  ActiveDecay        <- mbind(setNames(calcOutput("ActiveDecay",  landtype="crop",   aggregate = FALSE),"crop"),
                              setNames(calcOutput("ActiveDecay",  landtype="natveg", aggregate = FALSE),"natveg"))

  SlowDecay          <- mbind(setNames(calcOutput("SlowDecay",  landtype="crop",   aggregate = FALSE),"crop"),
                              setNames(calcOutput("SlowDecay",  landtype="natveg", aggregate = FALSE),"natveg"))

  PassiveDecay       <- mbind(setNames(calcOutput("PassiveDecay", landtype="crop",   aggregate = FALSE),"crop"),
                              setNames(calcOutput("PassiveDecay", landtype="natveg", aggregate = FALSE),"natveg"))

  Landuse            <- calcOutput("Landuse",       aggregate=FALSE)
  LanduseChange      <- calcOutput("LanduseChange", aggregate=FALSE)

  subpools     <- c("active","slow","passive")
  names        <- as.vector(outer(getNames(Landuse), subpools, paste, sep="."))
  SoilCarbon   <- SoilCarbonSteadyState <- Decay <- new.magpie(getCells(Landuse), getYears(Landuse), names, fill = 0)

  SoilCarbonSteadyState[,years,"active"]  <- ActiveSteadyState
  SoilCarbonSteadyState[,years,"slow"]    <- SlowSteadyState
  SoilCarbonSteadyState[,years,"passive"] <- PassiveSteadyState

  noCropCells               <- which(Landuse[,,"crop"]==0)
  #Clear cells with no Cropland ->
  for(sub in getNames(SoilCarbonSteadyState, dim=2)){
    SoilCarbonSteadyState[,,sub][noCropCells] <- 0
  }

  Decay[,years,"active"]                  <- ActiveDecay
  Decay[,years,"slow"]                    <- SlowDecay
  Decay[,years,"passive"]                 <- PassiveDecay
  Decay[Decay>1]                          <- 1

  if(init=="natveg"){
    SoilCarbon[,years[1],"active"]     <- mbind(ActiveSteadyState[,years[1],"natveg"], setNames(ActiveSteadyState[,years[1],"natveg"], "crop"))
    SoilCarbon[,years[1],"slow"]       <- mbind(SlowSteadyState[,years[1],"natveg"], setNames(SlowSteadyState[,years[1],"natveg"], "crop"))
    SoilCarbon[,years[1],"passive"]    <- mbind(PassiveSteadyState[,years[1],"natveg"], setNames(PassiveSteadyState[,years[1],"natveg"], "crop"))


  } else if(init=="lu"){
    SoilCarbon[,years[1],"active"]     <- ActiveSteadyState[,years[1],]
    SoilCarbon[,years[1],"slow"]       <- SlowSteadyState[,years[1],]
    SoilCarbon[,years[1],"passive"]    <- PassiveSteadyState[,years[1],]
  }

  #Clear cells with no Landuse -> no Soil
  noSoilCells               <- where(dimSums(Landuse, dim=3)==0)$true$regions
  SoilCarbon[noSoilCells,,] <- 0
  SoilCarbonSteadyState[noSoilCells,,] <- 0

  SoilCarbonTransfer      <- SoilCarbonInter       <- SoilCarbonRelease       <-  SoilCarbon
  SoilCarbonTransfer[,1,] <- SoilCarbonInter[,1,]  <- SoilCarbonRelease[,1,]  <- 0
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

    SoilCarbonRelease[,year_x,]  <- SoilCarbonInter[,year_x,] * Decay[,year_x,]

    print(year_x)
  }

  if(output=="reduced"){

    out <- SoilCarbon

  } else if(output=="full"){

    out <- mbind(add_dimension(SoilCarbon,            dim=3.1, add="var", nm="actualstate"),
                 add_dimension(SoilCarbonSteadyState, dim=3.1, add="var", nm="steadystate"),
                 add_dimension(Decay,                 dim=3.1, add="var", nm="decayrate"),
                 add_dimension(SoilCarbonTransfer,    dim=3.1, add="var", nm="carbontransfer"),
                 add_dimension(SoilCarbonInter,       dim=3.1, add="var", nm="interstate"))
  }

  return(list( x            = out,
               weight       = NULL,
               unit         = "Mt C",
               description  = "Carbon budget on croplands for historical period",
               isocountries = FALSE)
    )
}

