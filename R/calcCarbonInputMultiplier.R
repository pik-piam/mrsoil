#' @title calcCarbonInputMulitplier
#' @description This function compiles carbon inputs multipliers for steady state calculations for mineral soils
#'              using the steady-state method (Tier 2) of the 2019 Refinement to the 2006 IPP Guidelines
#'              for National Greenhouse Gas Inventories
#'
#' @return magpie object on choosen resolution
#' @author Kristine Karstens
#'
#' @examples
#' \dontrun{
#' calcOutput("CarbonInputMulitplier", aggregate = FALSE, input = generic")
#' }
#'
#' @importFrom magpiesets findset

calcCarbonInputMultiplier <- function() {

  soilParam      <- readSource("IPCCSoil", convert = FALSE)

  tillage2param  <- c(fulltill    = "f2_ft",
                      reducedtill = "f2_rt",
                      notill      = "f2_nt")
  f2struc2act <- setNames(soilParam[, , tillage2param], names(tillage2param))
  f4act2slo   <- calcOutput("TransferActive2Slow", aggregate = FALSE)

  inputProp <- NULL
  # get lignin to c and nitrogen to c values for all residues classes
  inputProp   <- mbind(inputProp,
                       calcOutput("ParamResidues", source = "IPCC+woody", aggregate = FALSE))
  # get lignin to c and nitrogen to c values for all manure classes
  inputProp   <- mbind(inputProp,
                       calcOutput("ParamManure", source = "IPCC", aggregate = FALSE))
  # get lignin to c and nitrogen to c values for generic input classes
  param     <- readSource("IPCC", subtype = "residues_table5p5b", convert = FALSE)
  generic   <- "Generic value for crops not indicated below"
  c2dm      <- 0.45
  inputProp <- mbind(inputProp,
                     setNames(param[, , generic] / c2dm,
                              paste0("generic.", gsub("_dm", "", getNames(param, dim = 2)))))

  # calculate carbon input multipliers
  out <- toolCarbonInputMultiplier(inputProp   = inputProp,
                                   soilParam   = soilParam,
                                   f4act2slo   = f4act2slo,
                                   f2struc2act = f2struc2act)

  weight <- collapseNames(calcOutput("ResBiomass", cellular = TRUE, aggregate = FALSE)[, "y1995", "c"])
  weight <- mbind(dimSums(weight, dim = 3.1),
                  setNames(dimSums(weight, dim = 3), "generic"),
                  collapseNames(calcOutput("CarbonManure", aggregate = FALSE)[, "y1995", "c"]))

  return(list(x            = out,
              weight       = weights,
              unit         = "tC per ha",
              description  = "Carbon input multiplier in tonnes carbon per hectare/ tonnes carbon per hectare
                              to all carbon subpools for different tillage and input types",
              isocountries = FALSE))
}
