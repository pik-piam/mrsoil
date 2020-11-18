#' @title calcSteadyState
#' @description This function wraps together the steady state for all sub-pool SOC stock for mineral soils
#' using the steady-state method (Tier 2) of the 2019 Refinement to the 2006 IPP Guidelines
#' for National Greenhouse Gas Inventories
#' @param tillage tillage type to de considered.
#'                   Default (histill) is historic tillage area shares based on no tillage areas from Porwollik together with rule based assumption;
#'                   'mixedtill' includes pure rule based assumptions.
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#' @examples
#' \dontrun{ calcOutput("SteadyState", aggregate = FALSE) }
#'
#' @import madrat
#' @import magclass

calcSteadyState <- function(tillage="histtill") {

  .steadystate <- function(alpha,decay,name) {
    x <- toolConditionalReplace(alpha/decay[,,name], "is.na()", 0)
    return(add_dimension(collapseNames(x),dim=3.2,add="subpool",nm=name))
  }

  # Load fractional transfer coefficients and more parameters
  param <- readSource("IPCCSoil", convert=FALSE)
  # f1 = stabilization efficiencies for metabolic decay products entering the active pool
  # f3 = stabilization efficiencies for structural decay products entering the slow pool
  # f5 = stabilization efficiencies for active pool decay products entering the passive pool
  # f6 = stabilization efficiencies for slow pool decay products entering the passive pool
  # f7 = stabilization efficiencies for slow pool decay products entering the active pool
  # f8 = stabilization efficiencies for passive pool decay products entering the active pool

  cell.f4_a2s      <- calcOutput("TransferActive2Slow", aggregate = FALSE)

  f2_struc2a <- function(param, tillage) {
    tillage2param  <- c(fulltill    = "f2_ft",
                        reducedtill = "f2_rt",
                        notill      = "f2_nt")
    tillage2area   <- c(mixedtill    = "ruleBased",
                        histtill     = "historicNoTill")

    f2              <- setNames(param[,,tillage2param], names(tillage2param))

    cell.till_areaShr <- calcOutput("TillageArea", tillage=tillage2area[tillage], aggregate = FALSE)
    # stabilization efficiencies for structural decay products entering the active pool if tillage is not known
    f2_struc2a.crop   <- dimSums(f2*cell.till_areaShr, dim=3)
    # backup: set all cell with no area info to full tillage (just in case it is needed)
    f2_struc2a.crop[f2_struc2a.crop==0] <- param[,,"f2_ft"]
    f2_struc2a.crop       <- setNames(f2_struc2a.crop,"crop")
    f2_struc2a.natveg     <- setNames(f2_struc2a.crop,"natveg")
    # stabilization efficiencies for structural decay products entering the active pool if tillage is not known
    f2_struc2a.natveg[,,] <- param[,,"f2_nt"]
    return(mbind(f2_struc2a.crop,f2_struc2a.natveg))
  }
  param.f2_struc2a   <- f2_struc2a(param,tillage)

  cell.input <- calcOutput("CarbonInput", aggregate = FALSE)
  decay      <- calcOutput("Decay", tillage=tillage, aggregate = FALSE)

  ### ActiveAlpha calculations ###
  ## Calculate all parts of carbon inflows to active pool
  # metabolic dead organic matter transferred to active SOC sub-pool
  cell.metabDOC_in <- cell.input[,,"metabDOC"] * param[,,"f1"]
  # structural dead organic matter transferred to active SOC sub-pool
  cell.strucDOC_in <- cell.input[,,"strucDOC"] * param.f2_struc2a
  # lignin carbon in slow and passive SOC sub-pool transferred back to active SOC sub-pool
  cell.lign_reflow <- cell.input[,,"ligninC"]  * param[,,"f3"] * (param[,,"f7"] + param[,,"f6"]*param[,,"f8"])
  # 1 - fraction of re-transferred SOC to active SOC sub-pool
  cell.frac_reflow <- 1 - cell.f4_a2s  * param[,,"f7"] -  param[,,"f5"] * param[,,"f8"] -
                          cell.f4_a2s  * param[,,"f6"] * param[,,"f8"]

  # Bring all carbon input to the active SOC sub-pool together
  ActiveAlpha <- dimSums(collapseNames((cell.metabDOC_in + cell.strucDOC_in + cell.lign_reflow) / cell.frac_reflow), dim=3.1)
  ActiveSteadyState <- .steadystate(ActiveAlpha,  decay, "active")

  ### SlowAlpha calculations ###
  ## Calculate all parts of carbon inflows
  # lignin carbon from carbon inputs transferred to slow SOC sub pool
  cell.lign_in   <- cell.input[,,"ligninC"] * param[,,"f3"]
  # SOC transferred from active to slow SOC sub-pool
  cell.aSOC_in   <- ActiveSteadyState * decay[,,"active"] * cell.f4_a2s
  # Bring all carbon input to the slow SOC sub-pool together
  SlowAlpha <- dimSums(collapseNames(cell.lign_in + cell.aSOC_in), dim=3.1)
  SlowSteadyState <- .steadystate(SlowAlpha,  decay, "slow")

  ### PassiveAlpha calculations ###
  ## Calculate all parts of carbon inflows
  # SOC transferred from active to passive SOC sub-pool
  cell.aSOC_in2   <- ActiveSteadyState * decay[,,"active"] * param[,,"f5"]
  # SOC transferred from slow to passive SOC sub-pool
  cell.sSOC_in    <- SlowSteadyState * decay[,,"slow"] * param[,,"f6"]
  # Bring all carbon input to the slow SOC sub-pool together
  PassiveAlpha     <- collapseNames(cell.aSOC_in2 + cell.sSOC_in)
  PassiveSteadyState <- .steadystate(PassiveAlpha,  decay, "passive")


  SoilCarbonSteadyState <-magpiesort(mbind(ActiveSteadyState,
                                           SlowSteadyState,
                                           PassiveSteadyState))

  return(list(
    x=SoilCarbonSteadyState,
    weight=NULL,
    unit="per yr",
    description="Decay rate for all SOC sub-pool per year",
    isocountries=FALSE))
}
