#' @title toolLanduseChange
#' @description Calculates the cellular MAgPIE landuse change area based on given landuse data and scenario
#'
#' @param landuse landuse data
#' @return List of magpie object with results on cellular level
#' @author Kristine Karstens

toolLanduseChange <- function(landuse){

  years         <- getYears(landuse, as.integer = TRUE)
  LanduseChange <- landuse[,years[2:length(years)],] - setYears(landuse[,years[2:length(years)]-1,], years[2:length(years)])
  LanduseChange <- mbind(add_dimension(LanduseChange, dim=3.1, add="change", nm="reduction"),
                         add_dimension(LanduseChange, dim=3.1, add="change", nm="expansion"))

  LanduseChange[,,"reduction"][LanduseChange[,,"reduction"]>0] <- 0
  LanduseChange[,,"reduction"]                                 <- (-1)*LanduseChange[,,"reduction"]
  LanduseChange[,,"expansion"][LanduseChange[,,"expansion"]<0] <- 0

  return(LanduseChange)
}

