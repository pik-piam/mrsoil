#' @title calcSandFrac
#' @description This function calculates the fraction of 0 - 30 cm soil mass that
#'              is sand (0.050 - 2mm particles)
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#' @seealso
#' \code{\link[mrcommons]{readSoilGrids}}
#'
#' @examples
#' \dontrun{
#' calcOutput("SandFrac", aggregate = FALSE)
#' }
calcSandFrac <- function() {

  sandfrac0  <- readSource("SoilGrids", subtype = "sandfrac_0", convert = "onlycorrect")
  sandfrac5  <- readSource("SoilGrids", subtype = "sandfrac_5", convert = "onlycorrect")
  sandfrac15 <- readSource("SoilGrids", subtype = "sandfrac_15", convert = "onlycorrect")
  sandfrac30 <- readSource("SoilGrids", subtype = "sandfrac_30", convert = "onlycorrect")

  sandfrac0to30 <- sandfrac0  * 1 / 12 +
                   sandfrac5  * 3 / 12 +
                   sandfrac15 * 5 / 12 +
                   sandfrac30 * 3 / 12

  # percent to dimensionless
  sandfrac0to30 <- sandfrac0to30 / 100

  getNames(sandfrac0to30) <- "sandfrac0to30"
  getYears(sandfrac0to30) <- NULL

  return(list(x            = sandfrac0to30,
              weight       = NULL,
              unit         = "",
              description  = "Fraction of 0-30cm soil mass that is sand, dimensionless",
              isocountries = FALSE))
}
