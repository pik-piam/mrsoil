#' @title calcCarbonManure
#' @description Calculates carbon input from manure for cropland.
#'
#' @return List of magpie object with results on country level, weight on country level, unit and description.
#' @author Kristine Karstens
#'
#' @examples
#' \dontrun{
#' calcOutput("CarbonManure")
#' }
#' @importFrom magclass dimCode
#' @importFrom stats quantile
#' @import mrcommons

calcCarbonManure <- function() {

  manureApplication  <- collapseNames(calcOutput("ManureRecyclingCroplandPast", products = "kli",
                                                 cellular = TRUE, aggregate = FALSE)[, , "c"])
  manureGrazing      <- collapseNames(calcOutput("Excretion", cellular = TRUE, attributes = "npkc",
                                                 aggregate = FALSE)[, , "stubble_grazing"][, , "c"])
  manureInput        <- (manureApplication + manureGrazing)
  cropland           <- dimSums(calcOutput("Croparea", cellular = TRUE, aggregate = FALSE), dim = 3)
  manureInput        <- manureInput / cropland
  manureInput        <- toolConditionalReplace(manureInput, conditions = c("is.na()", "<0"), replaceby = 0)
  manureInput        <- toolConditionalReplace(manureInput, conditions = c("is.infinite()"), replaceby = 0)
  # Cut high input values at 10 tC/ha
  manureInput        <- toolConditionalReplace(manureInput, conditions = "> 10", replaceby = 10)
  # Load parameters for lignin and nitrogen
  param              <- calcOutput("ParamManure",  input = "IPCC", aggregate = FALSE)

  kli          <- findset("kli")
  attributes   <- c("c", "LC", "NC")
  names        <- as.vector(outer(kli, attributes, paste, sep = "."))
  out          <- new.magpie(getCells(manureInput), getYears(manureInput), names, fill = 0)
  getSets(out) <- c(getSets(manureInput, fulldim = FALSE)[1:2], "kli.attributes")

  out[, , "c"]            <- manureInput
  out[, , c("NC", "LC")]  <- param
  getSets(out, fulldim = FALSE)[1] <- "x.y.iso"

  return(list(x            = out,
              weight       = NULL,
              unit         = "tC per ha, tN per tC, tLn per tC",
              description  = paste0("Carbon Input from Manure for livestock categories"),
              min          = 0,
              isocountries = FALSE))
}
