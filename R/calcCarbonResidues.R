#' @title calcCarbonResidues
#' @description Calculates carbon input from residues for cropland soils.
#'
#' @return List of magpie object with results on cellular level, weight on cellular level, unit and description.
#' @author Kristine Karstens
#' @examples
#'
#' \dontrun{
#' calcOutput("CarbonResidues")
#' }
#' @importFrom magclass dimCode
#' @import mrcommons
#' @importFrom stats quantile

calcCarbonResidues <- function(){

  ResidueRecyclingAg  <- collapseNames(calcOutput("ResFieldBalancePast", cellular=TRUE, aggregate = FALSE)[,,"recycle"][,,c("c","nr")])
  ResidueRecyclingBg  <- dimSums(calcOutput("ResBiomass", cellular=TRUE, aggregate = FALSE)[,,"bg"][,,c("c","nr")], dim=3.2)
  ResidueRecycling    <- collapseNames(ResidueRecyclingBg + ResidueRecyclingAg)
  Cropland            <- dimSums(calcOutput("Croparea", cellular=TRUE, aggregate=FALSE), dim=3)
  ResidueRecycling    <- ResidueRecycling/Cropland
  ResidueRecycling    <- toolConditionalReplace(ResidueRecycling, conditions = c("is.na()","<0","is.infinite()"), replaceby = 0)

  ResidueCNratio      <- toolConditionalReplace(ResidueRecycling[,,"c"]/ResidueRecycling[,,"nr"], conditions = "is.na()", replaceby = 1)

  ## Cut high input values at 10 tC/ha
  ResidueRecycling[,,"c"]  <- toolConditionalReplace(ResidueRecycling[,,"c"], conditions = "> 10", replaceby=10)
  ResidueRecycling[,,"nr"] <- ResidueRecycling[,,"c"] / ResidueCNratio


  attributes   <- c("c","LC","NC")
  names        <- as.vector(outer("res", attributes, paste, sep="."))
  out          <- new.magpie(getCells(ResidueRecycling), getYears(ResidueRecycling), names, fill = 0)
  getSets(out) <- c("iso","cell","t","inputs","attributes")

  out[,,"c"]   <- ResidueRecycling[,,"c"]
  out[,,"NC"]  <- ResidueRecycling[,,"nr"]/ResidueRecycling[,,"c"]
  out[,,"LC"]  <- 0.073 / 0.44  # (LC/dm / c/dm  =  LC/c)

  out <- toolConditionalReplace(out, conditions = c("is.na()","<0"), replaceby = 0)

  return(list(x=out,
              weight=NULL,
              unit="tC per ha, tN per tC, tLn per tC",
              description=paste0("Carbon Input from Residues to agricultural soils"),
              min = 0,
              isocountries = FALSE))
}
