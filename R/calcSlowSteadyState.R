#' @title calcSlowSteadyState
#' @description This function calculates the steady state slow sub-pool SOC stock for mineral soils
#' using the steady-state method (Tier 2) of the 2019 Refinement to the 2006 IPP Guidelines
#' for National Greenhouse Gas Inventories
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#' @param landtype 'crop' for cropland, 'natveg' for all the rest
#' @examples
#' \dontrun{ calcOutput("SlowSteadyState", aggregate = FALSE) }
#'
#' @import madrat
#' @import magclass

calcSlowSteadyState <- function(landtype="crop") {

  # Load fractional transfer coefficients and more parameters
  param <- readSource("IPCCSoil", convert=FALSE)
  param.f3_struc2s <- setYears(param[,,"f3"], NULL) # stabilization efficiencies for structural decay products entering the slow pool
  cell.f4_a2s      <- calcOutput("TransferActive2Slow", aggregate = FALSE)

  # Load inputs, active steady state stock and decay rate
  cell.input     <- calcOutput("CarbonInput", landtype=landtype, aggregate = FALSE)[,,"ligninC"]
  cell.ss_active <- calcOutput("ActiveSteadyState", landtype=landtype, aggregate = FALSE)
  cell.k_active  <- calcOutput("ActiveDecay", landtype=landtype, aggregate = FALSE)

  ## Calculate all parts of carbon inflows
  # lignin carbon from carbon inputs transferred to slow SOC sub pool
  cell.lign_in   <- cell.input * param.f3_struc2s
  # SOC transferred from active to slow SOC sub-pool
  cell.aSOC_in   <- cell.ss_active * cell.k_active * cell.f4_a2s

  cell.lign_in   <- collapseNames(cell.lign_in)
  cell.aSOC_in   <- collapseNames(cell.aSOC_in)

  # Bring all carbon input to the slow SOC sub-pool together
  cell.alpha     <- dimSums(cell.lign_in + cell.aSOC_in, dim=3)

  # Load decay rate
  cell.k_slow    <- calcOutput("SlowDecay", landtype=landtype, aggregate = FALSE)

  # Calculate long term equillibrium as input divided by decay rate
  cell.ss_slow   <- cell.alpha / cell.k_slow
  cell.ss_slow   <- toolConditionalReplace(cell.ss_slow[,sort(getItems(cell.ss_slow, dim=2)),], "is.na()", 0)

  return(list(
    x=cell.ss_slow,
    weight=NULL,
    unit="C tons per ha",
    description="Steady state slow sub-pool SOC stock in tonnes Carbon per ha",
    isocountries=FALSE))
}
