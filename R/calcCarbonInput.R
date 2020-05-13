#' @title calcCarbonInput
#' @description This function compiles carbon inputs as well as lignin and nitrogen content for mineral soils
#' using the steady-state method (Tier 2) of the 2019 Refinement to the 2006 IPP Guidelines
#' for National Greenhouse Gas Inventories
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#' @param landtype 'crop' for cropland, 'natveg' for all the rest
#' @examples
#' \dontrun{ calcOutput("CarbonInput", aggregate = FALSE) }
#'
#' @import madrat
#' @import magclass
#' @import mrcommons
#' @importFrom magpiesets findset

calcCarbonInput <- function(landtype="crop") {

  if(landtype=="crop"){

    Residues   <- calcOutput("CarbonResidues", aggregate = FALSE)
    Manure     <- calcOutput("CarbonManure", aggregate = FALSE)
    cell.input <- mbind(Residues, Manure)

  } else if(landtype=="natveg"){

    cell.input   <- calcOutput("CarbonLitter", aggregate = FALSE)
  }

  cell.input <- toolFillYears(cell.input, findset("past_all"))

  param <- readSource("IPCCSoil", convert=FALSE)
  param.metabfrac_intercept <- setYears(param[,,"sp1"], NULL) # empirical parameter to estimate metabolic fraction of residue input (intercept
  param.metabfrac_slope     <- setYears(param[,,"sp2"], NULL) # empirical parameter to estimate metabolic fraction of residue input (slope

  # Calculate metabolic dead organic carbon input
  cell.LC2NC       <- collapseNames(toolConditionalReplace(cell.input[,,"LC"]/cell.input[,,"NC"], c("is.na()","is.infinite()"), 0))
  cell.metabDOC    <- collapseNames(cell.input[,,"c"] * (param.metabfrac_intercept - param.metabfrac_slope * cell.LC2NC))

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
