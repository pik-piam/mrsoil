#' @title calcSOCDebt
#' @description This function calculates and return SOC stocks and debts from Soil Carbon Debt github repo
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#' @seealso
#'  \code{\link[mrvalidation]{readSoilCarbonDebt}}
#'
#' @examples
#'  \dontrun{ calcOutput("calcSOCDebt", aggregate = FALSE) }

calcSOCDebt <- function() {

  x <- toolCoord2Isocell(readSource("SoilCarbonDebt", convert = "onlycorrect"))
  x <- x[ , , "SOCS_noLU", invert = TRUE] - x[ , , "SOCS_noLU"]

  area  <- calcOutput("LUH2v2", landuse_types = "LUH2v2", irrigation = FALSE, cellular = TRUE, aggregate = FALSE)
  area  <- setYears(dimSums(area[, 2010, ], dim = 3), NULL)

  return(list(
    x            = x,
    weight       = area,
    unit         = "tons C per ha",
    description  = "Soil Carbon Debt according to Sanderman et al.",
    isocountries = FALSE))
}
