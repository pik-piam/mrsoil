#' @title calcLanduse
#' @description Calculates the cellular MAgPIE landuse area based on LUH2v2
#'
#' @param period select historical period (handed over to readLUH2v2)
#' @param output "total" or "change"
#' @return List of magpie object with results on cellular level, weight, unit and description.
#' @author Kristine Karstens
#' @examples
#' \dontrun{
#' calcOutput("Landuse", aggregate = FALSE)
#' }
#'

calcLanduse <- function(period = "states_1900to2010", output = "total") {

  landuse    <- readSource("LUH2v2", subtype = period, convert = "onlycorrect")
  mapping    <- toolGetMapping(type = "sectoral", where = "mappingfolder", name = "LUH2v2.csv")
  mapping$land[mapping$land != "crop"] <- "natveg"
  landuse    <- toolAggregate(landuse, mapping, dim = 3.1, from = "luh2v2", to = "land")
  landuse    <- landuse[, sort(getItems(landuse, dim = 2)), ]

  if (output == "change") {
    landuse <- toolLanduseChange(calcOutput("Landuse", period = period, aggregate = FALSE))
  }

  return(list(x      = landuse,
              weight = NULL,
              unit   = "Mha",
              description  = "Land use or Land-use change for crop and natveg land use set",
              isocountries = FALSE)
  )

}
