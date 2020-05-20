#' @title calcPassiveSteadyState
#' @description This function calculates the steady state passive sub-pool SOC stock for mineral soils
#' using the steady-state method (Tier 2) of the 2019 Refinement to the 2006 IPP Guidelines
#' for National Greenhouse Gas Inventories
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#' @param landtype 'crop' for cropland, 'natveg' for all the rest
#' @examples
#' \dontrun{ calcOutput("PassiveSteadyState", aggregate = FALSE) }
#'
#' @import madrat
#' @import magclass

calcPassiveSteadyState <- function(landtype="crop"){

  # Load fractional transfer coefficients and more parameters
  param <- readSource("IPCCSoil", convert=FALSE)
  param.f5_a2p              <- setYears(param[,,"f5"], NULL) # stabilization efficiencies for active pool decay products entering the passive pool
  param.f6_s2p              <- setYears(param[,,"f6"], NULL) # stabilization efficiencies for slow pool decay products entering the passive pool

  # Load inputs, active and slow steady state stocks and decay rates
  cell.input     <- calcOutput("CarbonInput", landtype=landtype, aggregate = FALSE)
  cell.ss_active <- calcOutput("ActiveSteadyState", landtype=landtype, aggregate = FALSE)
  cell.k_active  <- calcOutput("ActiveDecay", landtype=landtype, aggregate = FALSE)
  cell.ss_slow   <- calcOutput("SlowSteadyState", landtype=landtype, aggregate = FALSE)
  cell.k_slow    <- calcOutput("SlowDecay", landtype=landtype, aggregate = FALSE)

  ## Calculate all parts of carbon inflows
  # SOC transferred from active to passive SOC sub-pool
  cell.aSOC_in   <- cell.ss_active * cell.k_active * param.f5_a2p
  # SOC transferred from slow to passive SOC sub-pool
  cell.sSOC_in   <- cell.ss_slow * cell.k_slow * param.f6_s2p

  cell.aSOC_in   <- collapseNames(cell.aSOC_in)
  cell.sSOC_in   <- collapseNames(cell.sSOC_in)

  # Bring all carbon input to the slow SOC sub-pool together
  cell.alpha     <- dimSums(cell.aSOC_in + cell.sSOC_in, dim=3)

  # Load decay rate
  cell.k_passive <- calcOutput("PassiveDecay", landtype=landtype, aggregate = FALSE)

  # Calculate long term equillibrium as input divided by decay rate
  cell.ss_passive <- cell.alpha / cell.k_passive
  cell.ss_passive <- toolConditionalReplace(cell.ss_passive[,sort(getItems(cell.ss_passive, dim=2)),], "is.na()", 0)

  return(list(
    x=cell.ss_passive,
    weight=NULL,
    unit="C tons per ha",
    description="Steady state passive sub-pool SOC stock in tonnes Carbon per ha",
    isocountries=FALSE))
}
