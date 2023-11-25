#' @title calcCarbonInputMulitplier
#' @description This function compiles carbon inputs multipliers for steady state calculations for mineral soils
#' using the steady-state method (Tier 2) of the 2019 Refinement to the 2006 IPP Guidelines
#' for National Greenhouse Gas Inventories
#'
#' @param input switch between input types
#' @return magpie object on choosen resolution
#' @author Kristine Karstens
#'
#' @examples
#' \dontrun{
#' calcOutput("CarbonInputMulitplier", aggregate = FALSE, input = generic")
#' }
#'
#' @import madrat
#' @import magclass
#' @import mrcommons
#' @importFrom magpiesets findset

calcCarbonInputMultiplier <- function(input = "generic") {

  soilParam      <- readSource("IPCCSoil", convert = FALSE)

  tillage2param  <- c(fulltill    = "f2_ft",
                      reducedtill = "f2_rt",
                      notill      = "f2_nt")
  f2struc2act <- setNames(soilParam[, , tillage2param], names(tillage2param))
  f4act2slo   <- calcOutput("TransferActive2Slow", aggregate = FALSE)

  if(input == "residues") {
    # get lignin to c and nitrogen to c values for all residues classes
    inputProp   <- calcOutput("ParamResidues", source = "IPCC+woody", aggregate = FALSE)

  } else if (input == "manure") {
    # get lignin to c and nitrogen to c values for all manure classes
    inputProp   <- calcOutput("ParamManure", source = "IPCC", aggregate = FALSE)

  } else if (input == "generic") {
    # get lignin to c and nitrogen to c values for generic input classes
    param     <- readSource("IPCC", subtype = "residues_table5p5b", convert = FALSE)
    generic   <- "Generic value for crops not indicated below"
    c2dm      <- 0.45
    inputProp <- setNames(param[, , generic] / c2dm, gsub("_dm", "", getNames(param, dim = 2)))

  } else {
    stop("'input' parameter setting unknown. Choose between: residues, manure, generic")
  }

  # calculate carbon input multipliers
  out <- toolCarbonInputMultiplier(inputProp   = inputProp,
                                   soilParam   = soilParam,
                                   f4act2slo   = f4act2slo,
                                   f2struc2act = f2struc2act)

  # weights -> get data on residues and or manure as weight
  # what weights to use for generic input?

  return(list(x            = out,
              weight       = NULL,
              unit         = "tC per ha",
              description  = "Carbon input multiplier in tonnes carbon per hectare/ tonnes carbon per hectare
                              to all carbon subpools for different tillage and input types",
              isocountries = FALSE))
}
