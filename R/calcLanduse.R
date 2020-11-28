#' @title calcLanduse
#' @description Calculates the cellular MAgPIE landuse area based on LUH2v2 or LanduseInitialisation data.
#'
#' @param landuse_scen landuse configuration
#' @return List of magpie object with results on country or cellular level, weight, unit and description.
#' @author Kristine Karstens
#' @examples
#'
#' \dontrun{
#' calcOutput("Landuse")
#' }

calcLanduse <- function(landuse_scen="default"){

  Landuse <- calcOutput("LUH2v2", cellular=TRUE, selectyears="past_all", aggregate=FALSE)
  Landuse <- mbind(Landuse[,,"crop"], setNames(dimSums(Landuse[,,"crop",invert=TRUE], dim=3), "natveg"))

  Landuse <- Landuse[,sort(getItems(Landuse, dim=2)),]

  if(grepl("freeze", landuse_scen)){
    freeze_year <- as.integer(gsub("freeze","",landuse_scen))
    reset_years <- getYears(Landuse, as.integer=TRUE) >= freeze_year
    Landuse[,reset_years,] <- setYears(Landuse[,rep(freeze_year,sum(reset_years)),], getYears(Landuse[,reset_years,]))
  }

  return(list(x=Landuse,
              weight=NULL,
              unit="Mha",
              description="Land use change (reduction, expansion) for crop and natveg land use set",
              isocountries=FALSE)
  )

}

