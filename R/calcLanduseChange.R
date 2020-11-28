#' @title calcLanduseChange
#' @description Calculates the cellular MAgPIE landuse change area based on LUH2v2 or LanduseInitialisation data.
#'
#' @param landuse_scen landuse configuration
#' @return List of magpie object with results on country or cellular level, weight, unit and description.
#' @author Kristine Karstens
#' @examples
#'
#' \dontrun{
#' calcOutput("LanduseChange")
#' }

calcLanduseChange <- function(landuse_scen="default"){

  years <- sort(as.numeric(substring(findset("past_all"),2)))

  LanduseFirst <- calcOutput("LUH2v2", cellular=TRUE, selectyears="y1960", aggregate=FALSE)
  LanduseFirst <- mbind(LanduseFirst[,,"crop"], setNames(dimSums(LanduseFirst[,,"crop",invert=TRUE], dim=3), "natveg"))
  Landuse      <- calcOutput("Landuse", aggregate=FALSE)
  Landuse      <- mbind(LanduseFirst, Landuse)

  LanduseChange <- Landuse[,years[1:length(years)],]-setYears(Landuse[,years[1:length(years)]-1,],years[1:length(years)])

  LandReduction <- LandExpansion <- LanduseChange
  LandReduction[LandReduction>0] <- 0
  LandReduction                  <- (-1)*LandReduction
  LandExpansion[LandExpansion<0] <- 0

  out <- mbind(add_dimension(LandReduction, dim=3.1, add="change", nm="reduction"),
               add_dimension(LandExpansion, dim=3.1, add="change", nm="expansion"))

  if(grepl("freeze", landuse_scen)){
    freeze_year <- as.integer(gsub("freeze","",landuse_scen))
    reset_years <- getYears(out, as.integer=TRUE) >= freeze_year
    out[,reset_years,] <- 0
  }


  return(list(x=out,
              weight=NULL,
              unit="Mha",
              description="Land use change (reduction, expansion) for crop and natveg land use set",
              isocountries=FALSE)
  )
}

