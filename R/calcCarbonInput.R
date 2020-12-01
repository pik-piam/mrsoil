#' @title calcCarbonInput
#' @description This function compiles carbon inputs as well as lignin and nitrogen content for mineral soils
#' using the steady-state method (Tier 2) of the 2019 Refinement to the 2006 IPP Guidelines
#' for National Greenhouse Gas Inventories
#'
#' @param cfg run configuration
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#' @examples
#' \dontrun{ calcOutput("CarbonInput", aggregate = FALSE) }
#'
#' @import madrat
#' @import magclass
#' @import mrcommons
#' @importFrom magpiesets findset

calcCarbonInput <- function(cfg=NULL) {

  .prep <- function(x,landtype) {
    return(toolFillYears(add_dimension(x, dim=3.3, nm=landtype, add="landtype"), findset("past_soc")))
  }

  Residues   <- .prep(calcOutput("CarbonResidues", yieldscenario = cfg$yield, rec.scenario = cfg$rrecycle, res.scenario=cfg$residue, aggregate = FALSE),"crop")
  Manure     <- .prep(calcOutput("CarbonManure", scenario=cfg$manure, aggregate = FALSE),  "crop")
  Litter     <- .prep(calcOutput("CarbonLitter", litter_param=cfg$litter_param, climate_scen=cfg$climate, aggregate = FALSE),  "natveg")
  cell.input <- mbind(Residues, Manure, Litter)

  param <- readSource("IPCCSoil", convert=FALSE)
  param.metabfrac_intercept <- param[,,"sp1"] # empirical parameter to estimate metabolic fraction of residue input (intercept)
  param.metabfrac_slope     <- param[,,"sp2"] # empirical parameter to estimate metabolic fraction of residue input (slope)

  # Calculate metabolic dead organic carbon input
  cell.LC2NC       <- collapseNames(toolConditionalReplace(cell.input[,,"LC"]/cell.input[,,"NC"], c("is.na()","is.infinite()"), 0))
  cell.metabDOC    <- collapseNames(cell.input[,,"c"] * toolConditionalReplace(param.metabfrac_intercept - param.metabfrac_slope * cell.LC2NC, "<0", 0))

  # Calculate structural dead organic carbon input
  cell.strucDOC    <- collapseNames( cell.input[,,"c"] * (1 - cell.input[,,"LC"]) - cell.metabDOC )

  # Calculate lignin carbon input
  cell.lignC       <- collapseNames(cell.input[,,"c"] * cell.input[,,"LC"])

  out              <- mbind(add_dimension(collapseNames(cell.input[,,"c"]), dim=3.2, nm="totalC",   add="type"),
                            add_dimension(              cell.metabDOC,      dim=3.2, nm="metabDOC", add="type"),
                            add_dimension(              cell.strucDOC,      dim=3.2, nm="strucDOC", add="type"),
                            add_dimension(              cell.lignC,         dim=3.2, nm="ligninC",  add="type"))

  return(list(
    x=out,
    weight=NULL,
    unit="tC per ha",
    description="Carbon inputs in tonnes carbon per hectar (totals and sub input pools)",
    isocountries=FALSE))
}
