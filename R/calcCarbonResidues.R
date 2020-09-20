#' @title calcCarbonResidues
#' @description Calculates carbon input from residues for cropland soils.
#'
#' @param scenario define scenario switch for sensititvy analysis.
#'                'default' for historic assumptions
#'                'freezeXXXX' for frozen residue recycling rates from year XXXX
#'                'yieldsXXXX' for frozen yields for res calculations from year XXXX
#'
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


calcCarbonResidues <- function(scenario="default"){

  yieldscenario <- getOption("yield")
  scenario      <- getOption("residue")

  ResidueBiomass      <- calcOutput("ResBiomass", cellular=TRUE, aggregate = FALSE, scenario=yieldscenario)[,,c("c","nr")]
  kcr2kres            <- toolGetMapping("kcr_kres.csv",type="sectoral")
  ResidueBiomass      <- toolAggregate(ResidueBiomass, rel=kcr2kres, from="kcr", to="kres")
  ResidueRecyclingAg  <- collapseNames(calcOutput("ResFieldBalancePast", cellular=TRUE, products = "kres", aggregate = FALSE, scenario=yieldscenario)[,,"recycle"][,,c("c","nr")])

  if(grepl("freeze*", scenario)){
    RecycleShare   <- toolConditionalReplace(ResidueRecyclingAg/ResidueBiomass[,,"ag"], is.na(), 0)
    freeze_year    <- as.integer(gsub("freeze","",scenario))
    reset_years    <- getYears(RecycleShare, as.integer=TRUE) >= freeze_year
    # constant recycle share
    RecycleShare[,reset_years,] <- setYears(RecycleShare[,rep(freeze_year,sum(reset_years)),], getYears(RecycleShare[,reset_years,]))
    ResidueRecyclingAg          <- dimSums(RecycleShare + ResidueBiomass[,,"ag"], dim=3.2)
  } else {
    ResidueRecyclingAg <- dimSums(ResidueRecyclingAg, dim=3.2)
  }

  ResidueRecyclingBg  <- dimSums(ResidueBiomass[,,"bg"][,,c("c","nr")], dim=3.2)
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

  if(grepl("freeze*",scenario)){



  } else if(grepl("yields*",scenario)){


  }


  return(list(x=out,
              weight=NULL,
              unit="tC per ha, tN per tC, tLn per tC",
              description=paste0("Carbon Input from Residues to agricultural soils"),
              min = 0,
              isocountries = FALSE))
}
