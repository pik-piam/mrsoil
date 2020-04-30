#' @title calcCarbonManure
#' @description Calculates carbon input from manure for cropland.
#'
#' @return List of magpie object with results on country level, weight on country level, unit and description.
#' @author Kristine Karstens
#' @examples
#'
#' \dontrun{
#' calcOutput("CarbonManure")
#' }
#' @importFrom magclass dimCode
#' @importFrom stats quantile

calcCarbonManure <- function(){

  ManureApplication  <- collapseNames(calcOutput("ManureRecyclingCroplandPast", products = "kli", cellular = TRUE, aggregate = FALSE)[,,c("nr","c")])
  ManureGrazing      <- collapseNames(calcOutput("Excretion", cellular = TRUE, attributes = "npkc", aggregate = FALSE)[,,"stubble_grazing"][,,c("nr","c")])
  ManureInput        <- (ManureApplication + ManureGrazing)
  Cropland           <- dimSums(calcOutput("Croparea", cellular=TRUE, aggregate=FALSE), dim=3)
  ManureInput        <- ManureInput/Cropland
  ManureInput        <- toolConditionalReplace(ManureInput, conditions = c("is.na()","<0"), replaceby = 0)
  ManureInput        <- toolConditionalReplace(ManureInput, conditions = c("is.infinite()"), replaceby = 0)

  # Cut high input values at 95%-percentil
  ManureInput[,,"c"]  <- toolConditionalReplace(ManureInput[,,"c"], conditions = "> quantile(x, probs=0.95)", replaceby=eval(quantile(ManureInput[,,'c'], probs=0.95)))
  ManureInput[,,"nr"] <- toolConditionalReplace(ManureInput[,,"nr"], conditions = "> quantile(x, probs=0.95)", replaceby=eval(quantile(ManureInput[,,'nr'], probs=0.95)))

  param        <- readSource("IPCC", subtype="manure_table5p5c", convert=FALSE)

  kli <- findset("kli")
  attributes   <- c("c","LC","NC")
  names        <- as.vector(outer(kli, attributes, paste, sep="."))
  out          <- new.magpie(getCells(ManureInput), getYears(ManureInput), names, fill = 0)
  getSets(out) <- c("iso","cell","t","inputs","attributes")

  out[,,"c"]   <- ManureInput[,,"c"]
  out[,,"NC"]  <- ManureInput[,,"nr"] / ManureInput[,,"c"]
  out[,,"LC"]  <- 1 / 0.45 * param[,,"LC_dm"]/100

  out <- toolConditionalReplace(out, conditions = c("is.na()","<0"), replaceby = 0)

  return(list(x=out,
              weight=NULL,
              unit="tC per ha, tN per tC, tLn per tC",
              description=paste0("Carbon Input from Manure for livestock categories"),
              min = 0,
              isocountries = FALSE))
}
