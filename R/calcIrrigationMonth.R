#' @title calcIrrigationMonth
#' @description This function calculates the length of irrigation period crop type specific
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#' @examples
#' \dontrun{ calcOutput("IrrigationMonth", aggregate = FALSE) }
#'
#' @import madrat
#' @import magclass

calcIrrigationMonth <- function() {

  sdate <- readSource("LPJmL", subtype="LPJmL5:CRU_4.sdate", convert="onlycorrect")
  hdate <- readSource("LPJmL", subtype="LPJmL5:CRU_4.hdate", convert="onlycorrect")

}
