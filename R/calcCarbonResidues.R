#' @title calcCarbonResidues
#' @description Calculates carbon input from residues for cropland soils.
#'
#' @return List of magpie object with results on cellular level, weight on cellular level, unit and description.
#' @author Kristine Karstens
#'
#' @examples
#' \dontrun{
#' calcOutput("CarbonResidues")
#' }
#' @importFrom magclass dimCode
#' @importFrom stats quantile

calcCarbonResidues <- function() {

  residueBiomass       <- calcOutput("ResBiomass", cellular = TRUE, aggregate = FALSE)[, , "c"]
  kcr2kres             <- toolGetMapping("mappingCrop2Residue.csv", type = "sectoral", where = "mrcommons")
  residueBiomassBg     <- toolAggregate(residueBiomass, rel = kcr2kres, from = "kcr",
                                        to = "kres", dim = 3.2)[, , "bg"][, , "c"]
  callResFB            <- function() {
    calcOutput("ResFieldBalancePast", cellular = TRUE,
               products = "kres", aggregate = FALSE)
  }
  residueRecyclingAg   <- add_dimension(collapseNames(callResFB()[, , "recycle"][, , "c"]),
                                        dim = 3.1, add = "residues", nm = "ag")

  residueRecycling     <- residueBiomassBg + residueRecyclingAg
  cropland             <- dimSums(calcOutput("Croparea", cellular = TRUE, aggregate = FALSE), dim = 3)
  residueRecycling     <- collapseNames(residueRecycling / cropland)
  residueRecycling     <- toolConditionalReplace(residueRecycling, conditions = c("is.na()", "<0", "is.infinite()"),
                                                 replaceby = 0)
  ## Cut high input values at 10 tC/ha
  residueRecycling     <- toolConditionalReplace(residueRecycling, conditions = "> 10", replaceby = 10)
  # Load parameters for lignin and nitrogen and aggregate them to kres
  param                <- calcOutput("ParamResidues", aggregate = FALSE, input = "IPCC+woody")
  weight               <- collapseNames(dimSums(residueBiomass, dim = "residues"))
  param                <- toolAggregate(param,  weight = weight, rel = kcr2kres, from = "kcr", to = "kres", dim = 3.1)

  attributes   <- c("c", "LC", "NC")
  names        <- as.vector(outer(unique(kcr2kres$kres), attributes, paste, sep = "."))
  out          <- new.magpie(getCells(residueRecycling), getYears(residueRecycling), names, fill = 0)
  getSets(out)[3] <- c("inputs", "attributes")

  out[, , "c"]            <- residueRecycling
  out[, , c("LC", "NC")]  <- param
  getSets(out, fulldim = FALSE)[1] <- "x.y.iso"

  return(list(x            = out,
              weight       = NULL,
              unit         = "tC per ha, tN per tC, tLn per tC",
              description  = paste0("Carbon Input from Residues to agricultural soils"),
              min          = 0,
              isocountries = FALSE))
}
