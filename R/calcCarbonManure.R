#' @title calcCarbonManure
#' @description Calculates carbon input from manure for cropland.
#'
#' @param scenario define scenario switch for sensititvy analysis.
#'                'default' for historic assumptions
#'                'freezeXXXX' for frozen manure recycling rates from year XXXX
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
#' @import mrcommons

calcCarbonManure <- function(scenario="default"){

  scenario <- getOption("manure")

  ManureApplication  <- collapseNames(calcOutput("ManureRecyclingCroplandPast", products = "kli", cellular = TRUE, aggregate = FALSE)[,,c("nr","c")])
  ManureGrazing      <- collapseNames(calcOutput("Excretion", cellular = TRUE, attributes = "npkc", aggregate = FALSE)[,,"stubble_grazing"][,,c("nr","c")])
  ManureInput        <- (ManureApplication + ManureGrazing)
  Cropland           <- dimSums(calcOutput("Croparea", cellular=TRUE, aggregate=FALSE), dim=3)
  ManureInput        <- ManureInput/Cropland
  ManureInput        <- toolConditionalReplace(ManureInput, conditions = c("is.na()","<0"), replaceby = 0)
  ManureInput        <- toolConditionalReplace(ManureInput, conditions = c("is.infinite()"), replaceby = 0)

  param        <- readSource("IPCC", subtype="manure_table5p5c", convert=FALSE)

  ##  Cut high input values at 10 tC/ha
   ManureInput[,,"c"]  <- toolConditionalReplace(ManureInput[,,"c"],  conditions = "> 10", replaceby=10)
   ManureInput[,,"nr"] <- ManureInput[,,"c"] / param[,,"cn_ratio"]

  kli <- findset("kli")
  attributes   <- c("c","LC","NC")
  names        <- as.vector(outer(kli, attributes, paste, sep="."))
  out          <- new.magpie(getCells(ManureInput), getYears(ManureInput), names, fill = 0)
  getSets(out) <- c("iso","cell","t","inputs","attributes")

  out[,,"c"]   <- ManureInput[,,"c"]
  out[,,"NC"]  <- ManureInput[,,"nr"] / ManureInput[,,"c"]
  out[,,"LC"]  <- 1 / 0.44 * param[,,"LC_dm"]/100

  out <- toolConditionalReplace(out, conditions = c("is.na()","<0"), replaceby = 0)

  if(grepl("freeze", scenario)){

    freeze_year <- as.integer(gsub("freeze","",scenario))
    reset_years <- getYears(out, as.integer=TRUE) >= freeze_year
    out[,reset_years,] <- setYears(out[,rep(freeze_year,sum(reset_years)),], getYears(out[,reset_years,]))
    out[Cropland==0] <- 0
  }

  return(list(x=out,
              weight=NULL,
              unit="tC per ha, tN per tC, tLn per tC",
              description=paste0("Carbon Input from Manure for livestock categories"),
              min = 0,
              isocountries = FALSE))
}
