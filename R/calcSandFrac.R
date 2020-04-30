#' @title calcSandFrac
#' @description This function calculates the fraction of 0-30cm soil mass that is sand (0.050 - 2mm particles)
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#'@seealso
#' \code{\link{readSoilGrids}}
#'
#' @examples
#' \dontrun{ calcOutput("SandFrac", aggregate = FALSE) }
#'
#' @import madrat
#' @import magclass

calcSandFrac <- function() {

  sandfrac_0  <- readSource("SoilGrids", subtype="sandfrac_0", convert="onlycorrect")
  sandfrac_5  <- readSource("SoilGrids", subtype="sandfrac_5", convert="onlycorrect")
  sandfrac_15 <- readSource("SoilGrids", subtype="sandfrac_15", convert="onlycorrect")
  sandfrac_30 <- readSource("SoilGrids", subtype="sandfrac_30", convert="onlycorrect")

  sandfrac_0_30 <- sandfrac_0  * 1/12 +
                   sandfrac_5  * 3/12 +
                   sandfrac_15 * 5/12 +
                   sandfrac_30 * 3/12

  # percent to dimensionless
  sandfrac_0_30 <- sandfrac_0_30/100

  getNames(sandfrac_0_30) <- "sandfrac_0_30"
  getYears(sandfrac_0_30) <- NULL

  return(list(
    x=sandfrac_0_30,
    weight=NULL,
    unit="",
    description="Fraction of 0-30cm soil mass that is sand, dimensionless",
    isocountries=FALSE))
}
