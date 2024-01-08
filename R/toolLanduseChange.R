#' @title toolLanduseChange
#' @description Calculates the cellular MAgPIE landuse change area based on given landuse data and scenario
#'
#' @param landuse landuse data
#' @return List of magpie object with results on cellular level
#' @author Kristine Karstens

toolLanduseChange <- function(landuse) {

  years         <- getYears(landuse, as.integer = TRUE)
  landuseChange <-
    landuse[, years[2:length(years)], ] -
    setYears(landuse[, years[2:length(years)] - 1, ], years[2:length(years)])
  landuseChange <- mbind(add_dimension(landuseChange, dim = 3.1, add = "change", nm = "reduction"),
                         add_dimension(landuseChange, dim = 3.1, add = "change", nm = "expansion"))

  landuseChange[, , "reduction"][landuseChange[, , "reduction"] > 0] <- 0
  landuseChange[, , "reduction"]                                 <- (-1) * landuseChange[, , "reduction"]
  landuseChange[, , "expansion"][landuseChange[, , "expansion"] < 0] <- 0

  return(landuseChange)
}
