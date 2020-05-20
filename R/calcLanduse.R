#' @title calcLanduse
#' @description Calculates the cellular MAgPIE landuse area based on LUH2v2 or LanduseInitialisation data.
#'
#' @return List of magpie object with results on country or cellular level, weight, unit and description.
#' @author Kristine Karstens
#' @examples
#'
#' \dontrun{
#' calcOutput("Landuse")
#' }

calcLanduse <- function(){

  Landuse <- calcOutput("LUH2v2", cellular=TRUE, selectyears="past_all", aggregate=FALSE)
  Landuse <- mbind(Landuse[,,"crop"], setNames(dimSums(Landuse[,,"crop",invert=TRUE], dim=3), "natveg"))

  Landuse <- Landuse[,sort(getItems(Landuse, dim=2)),]

  return(list(x=Landuse,
              weight=NULL,
              unit="Mha",
              description="Land use change (reduction, expansion) for crop and natveg land use set",
              isocountries=FALSE)
  )

}

