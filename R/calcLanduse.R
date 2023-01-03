#' @title calcLanduse
#' @description Calculates the cellular MAgPIE landuse area based on LUH2v2 or LanduseInitialisation data.
#'
#' @param landuse_scen landuse configuration
#' @param output "total" or "change"
#' @return List of magpie object with results on country or cellular level, weight, unit and description.
#' @author Kristine Karstens
#' @examples
#'
#' \dontrun{
#' calcOutput("Landuse")
#' }
#'
#' @importFrom stringr str_match


calcLanduse <- function(landuse_scen="default", output="total"){

  if(grepl("spinup", landuse_scen)) states <- paste0("states", str_match(landuse_scen, "_\\d+to\\d+"))
  else                              states <- "states_1900to2010"

  Landuse    <- toolCoord2Isocell(readSource("LUH2v2", subtype = states, convert="onlycorrect"))
  mapping    <- toolGetMapping(type = "sectoral", name = "LUH2v2.csv")
  mapping$land[mapping$land!="crop"] <- "natveg"
  Landuse    <- toolAggregate(Landuse, mapping, dim = 3.1, from="luh2v2", to="land")
  Landuse    <- Landuse[,sort(getItems(Landuse, dim=2)),]

  if(grepl("freeze", landuse_scen)){
    freeze_year <- as.integer(gsub("freeze","",landuse_scen))
    reset_years <- getYears(Landuse, as.integer=TRUE) >= freeze_year
    Landuse[,reset_years,] <- setYears(Landuse[,rep(freeze_year,sum(reset_years)),], getYears(Landuse[,reset_years,]))
  }

  if(output=="change"){
    Landuse <- toolLanduseChange(calcOutput("Landuse", landuse_scen=landuse_scen, aggregate=FALSE))
  }

  return(list(x=Landuse,
              weight=NULL,
              unit="Mha",
              description="Land use or Land-use change for crop and natveg land use set",
              isocountries=FALSE)
  )

}
