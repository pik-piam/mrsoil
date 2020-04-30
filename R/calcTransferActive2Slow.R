#' @title calcTransferActive2Slow
#' @description This function calculates the fraction of active SOC sub-pool decay products
#' transferred to the slow SOC sub-pool for mineral soils using the steady-state method (Tier 2)
#' of the 2019 Refinement to the 2006 IPP Guidelines for National Greenhouse Gas Inventories
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#' @examples
#' \dontrun{ calcOutput("TransferActive2Slow", aggregate = FALSE) }
#'
#' @import madrat
#' @import magclass

calcTransferActive2Slow <- function() {

  param <- readSource("IPCCSoil", convert=FALSE)
  param.f5_a2p    <- setYears(param[,,"f5"], NULL) # stabilization efficiencies for active pool decay products entering the passive pool
  param.sand_intercept_a2s <- setYears(param[,,"f4par1"], NULL) # intersept term for sand effect on stabilization efficiencies for active pool decay products entering the slow pool
  param.sand_slope_a2s     <- setYears(param[,,"f4par2"], NULL) # slope term for sand effect on stabilization efficiencies for active pool decay products entering the slow pool

  cell.sand_frac <- calcOutput("SandFrac", aggregate = FALSE)
  cell.f4_a2s    <- 1 - param.f5_a2p - (param.sand_intercept_a2s + param.sand_slope_a2s * cell.sand_frac)

  return(list(
    x=cell.f4_a2s ,
    weight=NULL,
    unit="",
    description="Fraction of active SOC sub-pool decay products transferred to the slow SOC sub-pool",
    isocountries=FALSE))
}
