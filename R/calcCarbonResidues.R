#' @title calcCarbonResidues
#' @description Calculates carbon input from residues for cropland soils.
#'
#' @param yieldscenario yield scenario
#' @param rec.scenario recycling scenario
#' @param res.scenario residue scenario
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


calcCarbonResidues <- function(yieldscenario = "default", rec.scenario = "default", res.scenario="default"){

  .setdefault <- function(x) {
    if(is.null(x)) x <- "default"
    return(x)
  }
  yieldscenario <- .setdefault(yieldscenario)
  rec.scenario  <- .setdefault(rec.scenario)
  res.scenario  <- .setdefault(res.scenario)

  ResidueBiomass      <- calcOutput("ResBiomass", cellular=TRUE, aggregate = FALSE, scenario=yieldscenario)[,,c("c","nr")]
  kcr2kres            <- toolGetMapping("mappingCrop2Residue.csv", type="sectoral", where="mrcommons")
  ResidueBiomass      <- toolAggregate(ResidueBiomass, rel=kcr2kres, from="kcr", to="kres", dim=3.2)
  ResidueRecyclingAg  <- collapseNames(calcOutput("ResFieldBalancePast", cellular=TRUE, products = "kres", aggregate = FALSE, scenario=yieldscenario)[,,"recycle"][,,c("c","nr")])

  if(grepl("freeze", rec.scenario)){
    RecycleShare   <- toolConditionalReplace(ResidueRecyclingAg/ResidueBiomass[,,"ag"], "is.na()", 0)
    freeze_year    <- as.integer(gsub("freeze","",rec.scenario))
    RecycleShare   <- toolFreezeEffect(RecycleShare,freeze_year)
    ResidueRecyclingAg <- dimSums(RecycleShare * ResidueBiomass[,,"ag"], dim=3.1)
  } else {
    ResidueRecyclingAg <- dimSums(ResidueRecyclingAg, dim=3.1)
  }

  ResidueRecyclingBg  <- dimSums(ResidueBiomass[,,"bg"][,,c("c","nr")], dim=3.2)
  ResidueRecycling    <- mbind(ResidueRecyclingBg,add_dimension(ResidueRecyclingAg, dim=3.1, add="residues", nm="ag"))
  Cropland            <- dimSums(calcOutput("Croparea", cellular=TRUE, aggregate=FALSE), dim=3)
  ResidueRecycling    <- ResidueRecycling/Cropland
  ResidueRecycling    <- toolConditionalReplace(ResidueRecycling, conditions = c("is.na()","<0","is.infinite()"), replaceby = 0)

  ResidueCNratio      <- toolConditionalReplace(ResidueRecycling[,,"c"]/ResidueRecycling[,,"nr"], conditions = "is.na()", replaceby = 0.44/0.0083) # generic value

  if(grepl("freeze", res.scenario)){
    freeze_year <- as.integer(gsub("freeze","",res.scenario))
    ResidueRecycling[,,"ag.c"] <- toolFreezeEffect(ResidueRecycling[,,"ag.c"],freeze_year, constrain="first_use")
    ResidueRecycling[,,"bg.c"] <- toolFreezeEffect(ResidueRecycling[,,"bg.c"],freeze_year, constrain="first_use")
    ResidueRecycling[,,"c"][Cropland==0] <- 0
  }

  ## Cut high input values at 10 tC/ha
  ResidueRecycling[,,"c"]  <- toolConditionalReplace(ResidueRecycling[,,"c"], conditions = "> 5", replaceby=5)
  ResidueRecycling[,,"nr"] <- ResidueRecycling[,,"c"] / ResidueCNratio

  attributes   <- c("c","LC","NC")
  names        <- as.vector(outer(c("ag","bg"), attributes, paste, sep="."))
  out          <- new.magpie(getCells(ResidueRecycling), getYears(ResidueRecycling), names, fill = 0)
  getSets(out) <- c("iso","cell","t","inputs","attributes")

  out[,,"c"]   <- ResidueRecycling[,,"c"]
  out[,,"NC"]  <- ResidueRecycling[,,"nr"]/ResidueRecycling[,,"c"]
  out[,,"LC"]  <- 0.073 / 0.44  # (LC/dm / c/dm  =  LC/c)

  out <- toolConditionalReplace(out, conditions = c("is.na()","<0","is.nan()"), replaceby = 0)


  return(list(x=out,
              weight=NULL,
              unit="tC per ha, tN per tC, tLn per tC",
              description=paste0("Carbon Input from Residues to agricultural soils"),
              min = 0,
              isocountries = FALSE))
}
