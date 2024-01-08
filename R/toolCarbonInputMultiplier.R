#' @title toolCarbonInputMulitplier
#' @description This function compiles carbon inputs multipliers for steady state calculations for mineral soils
#'              using the steady-state method (Tier 2) of the 2019 Refinement to the 2006 IPP Guidelines
#'              for National Greenhouse Gas Inventories
#'
#' @param inputProp   input properties: lignin to c (LC) and nitrogen to c (NC) ratio
#'                    on cellular or global resolution, with or without time resolution
#' @param soilParam   soil model parameters (via readSource("IPCCSoil"))
#' @param f4act2slo   stabilization efficiencies for active pool decay products entering the passive pool
#'                    (this factor is sand content depending and therefore spatial explicit)
#' @param f2struc2act stabilization efficiencies for structural decay products entering the active pool
#'                    (this factor is tillage type depending and can but not must be spatial explicit)
#'
#' @return magpie object in input resolution
#' @author Kristine Karstens
#'
#' @export

toolCarbonInputMultiplier <- function(inputProp, soilParam, f4act2slo, f2struc2act) {

  coords <- FALSE
  if (any(hasCoords(inputProp) | hasCoords(soilParam) | hasCoords(f4act2slo) | hasCoords(f2struc2act))) {
    coords <- TRUE
  }
  # empirical parameter to estimate metabolic fraction of residue input (intercept)
  paramMetabfracIntercept <- soilParam[, , "sp1"]
  # empirical parameter to estimate metabolic fraction of residue input (slope)
  paramMetabfracSlope     <- soilParam[, , "sp2"]
  # f1 = stabilization efficiencies for metabolic decay products entering the active pool
  # f3 = stabilization efficiencies for structural decay products entering the slow pool
  # f5 = stabilization efficiencies for active pool decay products entering the passive pool
  # f6 = stabilization efficiencies for slow pool decay products entering the passive pool
  # f7 = stabilization efficiencies for slow pool decay products entering the active pool
  # f8 = stabilization efficiencies for passive pool decay products entering the active pool

  ############################################################
  ########## metabolic dead organic carbon           #########
  ############################################################
  # calculate metabolic dead organic carbon input multiplier
  lc2nc    <- collapseNames(toolConditionalReplace(inputProp[, , "LC"] / inputProp[, , "NC"],
                                                   c("is.na()", "is.infinite()"), 0))
  metabDOC <- collapseNames(toolConditionalReplace(paramMetabfracIntercept -
                                                     paramMetabfracSlope * lc2nc, "<0", 0))
  # correct for too big metabolic carbon input multiplier
  metabDOC <- pmin(metabDOC, (1 - inputProp[, , "LC"]))

  ############################################################
  ########## structural dead organic carbon          #########
  ############################################################
  # calculate structural dead organic carbon (non-lignin part) input multiplier
  strucDOC  <- collapseNames((1 - inputProp[, , "LC"]) - metabDOC)
  # calculate structural dead organic carbon (lignin part) input multiplier
  lignC     <- collapseNames(inputProp[, , "LC"])

  #################################################
  ### carbon input multiplier active pool       ###
  #################################################
  ## calculate all parts of carbon inflows to active pool
  activeIn <-
    # metabolic dead organic matter transferred to active SOC sub-pool
    metabDOC * soilParam[, , "f1"] +
    # structural dead organic matter transferred to active SOC sub-pool
    strucDOC * f2struc2act +
    # lignin carbon in slow and passive SOC sub-pool transferred back to active SOC sub-pool
    lignC  * soilParam[, , "f3"] * (soilParam[, , "f7"] + soilParam[, , "f6"] * soilParam[, , "f8"])
  # 1 - fraction of re-transferred SOC to active SOC sub-pool
  fracReflow <-
    1 - f4act2slo  * soilParam[, , "f7"] -
    soilParam[, , "f5"] * soilParam[, , "f8"] -
    f4act2slo  * soilParam[, , "f6"] * soilParam[, , "f8"]
  # Bring all carbon input to the active SOC sub-pool together
  activeIn <- collapseNames((activeIn) / fracReflow)

  #################################################
  ### carbon input multiplier slow pool         ###
  #################################################
  ## calculate all parts of carbon inflows to slow pool
  slowIn <-
    # lignin carbon from carbon inputs transferred to slow SOC sub pool
    lignC  * soilParam[, , "f3"] +
    # SOC transferred from active to slow SOC sub-pool
    activeIn * f4act2slo
  slowIn <- collapseNames(slowIn)

  #################################################
  ### carbon input multiplier passive pool      ###
  #################################################
  ## Calculate all parts of carbon inflows
  passiveIn <-
    # SOC transferred from active to passive SOC sub-pool
    activeIn * soilParam[, , "f5"] +
    # SOC transferred from slow to passive SOC sub-pool
    slowIn * soilParam[, , "f6"]
  passiveIn <- collapseNames(passiveIn)

  .prepareOut <- function(x, nm) {
    add_dimension(collapseNames(x), dim = 3.1, add = "pool", nm = nm)
  }
  out <-  mbind(.prepareOut(activeIn,  nm = "active"),
                .prepareOut(slowIn,    nm = "slow"),
                .prepareOut(passiveIn, nm =  "passive"))

  if (coords) getSets(out, fulldim = FALSE)[1] <- "x.y.iso"

  return(out)
}
