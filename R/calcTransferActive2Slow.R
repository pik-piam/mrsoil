#' @title calcTransferActive2Slow
#' @description This function calculates the fraction of active SOC sub-pool decay products
#' transferred to the slow SOC sub-pool for mineral soils using the steady-state method (Tier 2)
#' of the 2019 Refinement to the 2006 IPP Guidelines for National Greenhouse Gas Inventories
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#' @examples
#' \dontrun{
#' calcOutput("TransferActive2Slow", aggregate = FALSE)
#' }
#'
#' @import madrat
#' @import magclass

calcTransferActive2Slow <- function() {

  param <- readSource("IPCCSoil", convert = FALSE)
  # stabilization efficiencies for active pool decay products entering the passive pool
  paramF5a2p    <- setYears(param[, , "f5"], NULL)
  # intersept term for sand effect on stabilization efficiencies for active pool decay products entering the slow pool
  paramSandInterceptA2s <- setYears(param[, , "f4par1"], NULL)
  # slope term for sand effect on stabilization efficiencies for active pool decay products entering the slow pool
  paramSandSlopeA2s     <- setYears(param[, , "f4par2"], NULL)

  cellSandFrac <- calcOutput("SandFrac", aggregate = FALSE)
  cellF4a2s    <- 1 - paramF5a2p - (paramSandInterceptA2s + paramSandSlopeA2s * cellSandFrac)
  cellF4a2s    <- setNames(collapseDim(cellF4a2s), "f4act2slo")
  getSets(cellF4a2s) <- getSets(cellSandFrac)

  return(list(x            = cellF4a2s,
              weight       = NULL,
              unit         = "",
              description  = "Fraction of active SOC sub-pool decay products transferred to the slow SOC sub-pool",
              isocountries = FALSE))
}
