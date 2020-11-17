#' @title calcActiveSteadyState
#' @description This function calculates the steady state active sub-pool SOC stock for mineral soils
#' using the steady-state method (Tier 2) of the 2019 Refinement to the 2006 IPP Guidelines
#' for National Greenhouse Gas Inventories
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#' @param tillage tillage type to de considered.
#'                   Default (histill) is historic tillage area shares based on no tillage areas from Porwollik together with rule based assumption;
#'                   'mixedtill' includes pure rule based assumptions.
#' @examples
#' \dontrun{ calcOutput("ActiveSteadyState", aggregate = FALSE) }
#'
#' @import madrat
#' @import magclass

calcActiveSteadyState <- function(tillage="histtill"){

  # Load fractional transfer coefficients and more parameters
  param <- readSource("IPCCSoil", convert=FALSE)
  param.f1_metab2a          <- param[,,"f1"] # stabilization efficiencies for metabolic decay products entering the active pool
  param.f3_struc2s          <- param[,,"f3"] # stabilization efficiencies for structural decay products entering the slow pool
  param.f5_a2p              <- param[,,"f5"] # stabilization efficiencies for active pool decay products entering the passive pool
  param.f6_s2p              <- param[,,"f6"] # stabilization efficiencies for slow pool decay products entering the passive pool
  param.f7_s2a              <- param[,,"f7"] # stabilization efficiencies for slow pool decay products entering the active pool
  param.f8_p2a              <- param[,,"f8"] # stabilization efficiencies for passive pool decay products entering the active pool
  cell.f4_a2s               <- calcOutput("TransferActive2Slow", aggregate = FALSE)

  f2_struc2a <- function(param, tillage) {
    tillage2param       <- c(fulltill    = "f2_ft",
                             reducedtill = "f2_rt",
                             notill      = "f2_nt")
    tillage2area        <- c(mixedtill    = "ruleBased",
                             histtill     = "historicNoTill")

    f2                  <- setNames(param[,,tillage2param], names(tillage2param))

    cell.till_areaShr   <- calcOutput("TillageArea", tillage=tillage2area[tillage], aggregate = FALSE)
    param.f2_struc2a.crop    <- dimSums(f2*cell.till_areaShr, dim=3) # stabilization efficiencies for structural decay products entering the active pool if tillage is not known
    param.f2_struc2a.crop[param.f2_struc2a.crop==0] <- param[,,"f2_ft"]   # backup: set all cell with no area info to full tillage (just in case it is needed)
    param.f2_struc2a.crop <- setNames(param.f2_struc2a.crop,"crop")
    param.f2_struc2a.natveg <- param.f2_struc2a.crop
    param.f2_struc2a.natveg[,,] <- param[,,"f2_nt"] # stabilization efficiencies for structural decay products entering the active pool if tillage is not known
    param.f2_struc2a.natveg <- setNames(param.f2_struc2a.natveg,"natveg")
    return(mbind(param.f2_struc2a.crop,param.f2_struc2a.natveg))
  }
  param.f2_struc2a   <- f2_struc2a(param,tillage)

  cell.input <- calcOutput("CarbonInput", aggregate = FALSE)

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
  cell.alpha       <- dimSums((cell.metabDOC_in + cell.strucDOC_in + cell.lign_reflow) / cell.frac_reflow, dim=3.1)

  # Load decay rate
  cell.k_active    <- calcOutput("ActiveDecay", tillage=tillage, aggregate = FALSE)

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
