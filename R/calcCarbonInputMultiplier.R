#' @title calcCarbonInputMulitplier
#' @description This function compiles carbon inputs multipliers for steady state calculations for mineral soils
#'              using the steady-state method (Tier 2) of the 2019 Refinement to the 2006 IPP Guidelines
#'              for National Greenhouse Gas Inventories
#'
#' @param inputType switch between 'kcr' (default), or 'kli', 'generic'
#'
#' @return magpie object on choosen resolution
#' @author Kristine Karstens
#'
#' @examples
#' \dontrun{
#' calcOutput("CarbonInputMulitplier", aggregate = FALSE, input = generic)
#' }
#'
#' @importFrom magpiesets findset

calcCarbonInputMultiplier <- function(inputType = "kcr") {

  soilParam      <- readSource("IPCCSoil", convert = FALSE)

  tillage2param  <- c(fulltill    = "f2_ft",
                      reducedtill = "f2_rt",
                      notill      = "f2_nt")
  f2struc2act <- setNames(soilParam[, , tillage2param], names(tillage2param))
  f4act2slo   <- calcOutput("TransferActive2Slow", aggregate = FALSE)

  # get lignin to c and nitrogen to c values for input classes
  if (inputType == "kcr") {
    # get lignin to c and nitrogen to c values for crop type residue classes
    inputProp   <- calcOutput("ParamResidues", input = "IPCC+woody", aggregate = FALSE)
  } else if (inputType == "kli") {
    # get lignin to c and nitrogen to c values for all manure classes
    inputProp   <- calcOutput("ParamManure", input = "IPCC", aggregate = FALSE)
  } else if (inputType == "generic") {
    # get lignin to c and nitrogen to c values for generic input classes
    param     <- readSource("IPCC", subtype = "residues_table5p5b", convert = FALSE)
    generic   <- "Generic value for crops not indicated below"
    c2dm      <- 0.45
    inputProp <- setNames(param[, , generic] / c2dm,
                          paste0("generic.", gsub("_dm", "", getNames(param, dim = 2))))
  } else {
    stop("inputType setting unknown.")
  }

  # calculate carbon input multipliers for crop-related inputs
  out <- toolCarbonInputMultiplier(inputProp   = inputProp,
                                   soilParam   = soilParam,
                                   f4act2slo   = f4act2slo,
                                   f2struc2act = f2struc2act)

  # get weights
  if (inputType %in% c("kcr", "generic")) {
    weight <- collapseNames(calcOutput("ResBiomass", cellular = TRUE, aggregate = FALSE)[, "y1995", "c"])
    if (inputType == "kcr") {
      weight <- dimSums(weight, dim = 3.1)                       # for kcr
    } else {
      weight <- setNames(dimSums(weight, dim = 3), "generic")    # for 'generic'
    }
  } else if (inputType == "kli") {
    weight <- collapseNames(calcOutput("CarbonManure", aggregate = FALSE)[, "y1995", "c"])
  } else {
    stop("inputType setting unknown.")
  }

  weight <- weight + 10^-10 # important for empty categories such as betr, begr

  getSets(out,    fulldim = FALSE)[1] <- "x.y.iso"
  getSets(weight, fulldim = FALSE)[1] <- "x.y.iso"

  return(list(x            = out,
              weight       = weight,
              unit         = "tC per ha",
              description  = "Carbon input multiplier in tonnes carbon per hectare / tonnes carbon per hectare
                              to all carbon subpools for different tillage and input (manure and residue) types",
              isocountries = FALSE))
}
