#' @title calcActiveSteadyState
#' @description This function calculates the steady state active sub-pool SOC stock for mineral soils
#' using the steady-state method (Tier 2) of the 2019 Refinement to the 2006 IPP Guidelines
#' for National Greenhouse Gas Inventories
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#' @param landtype 'crop' for cropland, 'natveg' for all the rest
#' @examples
#' \dontrun{ calcOutput("ActiveSteadyState", aggregate = FALSE) }
#'
#' @import madrat
#' @import magclass

calcActiveSteadyState <- function(landtype="crop"){

  # Load fractional transfer coefficients and more parameters
  param <- readSource("IPCCSoil", convert=FALSE)
  param.f1_metab2a          <- setYears(param[,,"f1"], NULL) # stabilization efficiencies for metabolic decay products entering the active pool
  param.f3_struc2s          <- setYears(param[,,"f3"], NULL) # stabilization efficiencies for structural decay products entering the slow pool
  param.f5_a2p              <- setYears(param[,,"f5"], NULL) # stabilization efficiencies for active pool decay products entering the passive pool
  param.f6_s2p              <- setYears(param[,,"f6"], NULL) # stabilization efficiencies for slow pool decay products entering the passive pool
  param.f7_s2a              <- setYears(param[,,"f7"], NULL) # stabilization efficiencies for slow pool decay products entering the active pool
  param.f8_p2a              <- setYears(param[,,"f8"], NULL) # stabilization efficiencies for passive pool decay products entering the active pool
  cell.f4_a2s               <- calcOutput("TransferActive2Slow", aggregate = FALSE)

  if(landtype=="crop"){

    tillage2param       <- c(fulltill    = "f2_ft",
                             reducedtill = "f2_rt",
                             notill      = "f2_nt")

    f2                  <- setNames(param[,,tillage2param], names(tillage2param))

    tillage2area        <- c(mixedtill    = "ruleBased",
                             histtill     = "historicNoTill")

    cell.till_areaShr   <- calcOutput("TillageArea", tillage=tillage2area[getOption("tillage")], aggregate = FALSE)
    param.f2_struc2a    <- dimSums(f2*cell.till_areaShr, dim=3) # stabilization efficiencies for structural decay products entering the active pool if tillage is not known
    param.f2_struc2a[param.f2_struc2a==0] <- param[,,"f2_ft"]   # backup: set all cell with no area info to full tillage (just in case it is needed)


  } else if(landtype=="natveg"){
    param.f2_struc2a        <- setYears(param[,,"f2_nt"], NULL) # stabilization efficiencies for structural decay products entering the active pool if tillage is not known
  }

  cell.input <- calcOutput("CarbonInput", landtype=landtype, aggregate = FALSE)

  ## Calculate all parts of carbon inflows to active pool

  # metabolic dead organic matter transferred to active SOC sub-pool
  cell.metabDOC_in <- cell.input[,,"metabDOC"] * param.f1_metab2a
  # structural dead organic matter transferred to active SOC sub-pool
  cell.strucDOC_in <- cell.input[,,"strucDOC"] * param.f2_struc2a
  # lignin carbon in slow and passive SOC sub-pool transferred back to active SOC sub-pool
  cell.lign_reflow <- cell.input[,,"ligninC"]  * param.f3_struc2s * (param.f7_s2a + param.f6_s2p*param.f8_p2a)
  # 1 - fraction of re-transferred SOC to active SOC sub-pool
  cell.frac_reflow <- 1 - cell.f4_a2s  * param.f7_s2a -
                          param.f5_a2p * param.f8_p2a -
                          cell.f4_a2s  * param.f6_s2p * param.f8_p2a

  cell.metabDOC_in <- collapseNames(cell.metabDOC_in)
  cell.strucDOC_in <- collapseNames(cell.strucDOC_in)
  cell.lign_reflow <- collapseNames(cell.lign_reflow)
  cell.frac_reflow <- collapseNames(cell.frac_reflow)

  # Bring all carbon input to the active SOC sub-pool together
  cell.alpha       <- dimSums((cell.metabDOC_in + cell.strucDOC_in + cell.lign_reflow) / cell.frac_reflow, dim=3)

  # Load decay rate
  cell.k_active    <- calcOutput("ActiveDecay", landtype=landtype, aggregate = FALSE)

  # Calculate long term equillibrium as input divided by decay rate
  cell.ss_active   <- cell.alpha / cell.k_active
  cell.ss_active   <- toolConditionalReplace(cell.ss_active[,sort(getItems(cell.ss_active, dim=2)),], "is.na()", 0)

  return(list(
    x=cell.ss_active,
    weight=NULL,
    unit="C tons per ha",
    description="Steady state active sub-pool SOC stock in tonnes Carbon per ha",
    isocountries=FALSE))
}
