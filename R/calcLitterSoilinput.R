#' @title calcLitterSoilinput
#' @description Calculates Carbon Input from litter to the soil pools of the three pool model (Tier 2 IPCC)
#'
#' @param lpjmlNatveg Switch between LPJmL natveg versionstop
#' @param climatetype Switch between different climate scenarios
#' @param fixFpc      if TRUE using fixed (old) fpc data
#'
#' @return List of magpie object with results on cellular level, weight on cellular level, unit and description.
#' @author Kristine Karstens
#' @examples
#' \dontrun{
#' calcOutput("calcLitterSoilinput", aggregate = FALSE)
#' }

calcLitterSoilinput <-  function(lpjmlNatveg = "LPJmL4_for_MAgPIE_44ac93de",
                                 climatetype = "GSWP3-W5E5:historical",
                                 fixFpc      = FALSE) {

  # Load soil parameter collection
  soilParam   <- readSource("IPCCSoil", convert = FALSE)
  # Load fractional transfer coefficients for active to slow (sand fraction depending)
  f4act2slo   <- calcOutput("TransferActive2Slow", aggregate = FALSE)
  # Stabilization efficiencies for structural decay products entering the active pool
  # for natural vegetation is no tillage
  f2struc2act <- collapseDim(soilParam[, , "f2_nt"])

  litter <- calcOutput("CarbonLitter", lpjmlNatveg = lpjmlNatveg, climatetype = climatetype,
                       mode = "magpieInput", fixFpc = fixFpc, aggregate = FALSE)

  alpha <- dimSums(toolCarbonInputMultiplier(inputProp = litter[, , c("LC", "NC")],
                                             soilParam = soilParam,
                                             f4act2slo = f4act2slo,
                                             f2struc2act = f2struc2act) *
                     collapseDim(litter[, , "c"]), dim = 3.2)

  alpha <- toolConditionalReplace(alpha, conditions = c("is.na()", "<0", "is.infinite()"), replaceby = 0)

  weight <- calcOutput("LanduseInitialisation", aggregate = FALSE, cellular = TRUE, years = "y1995")
  weight <- collapseDim(dimSums(weight[, , "crop", invert = TRUE], dim = 3))

  getSets(alpha,    fulldim = FALSE)[1] <- "x.y.iso"
  getSets(weight, fulldim = FALSE)[1] <- "x.y.iso"

  return(list(x            = alpha,
              weight       = weight + 10^-10,
              unit         = "tC per ha",
              description  = paste0("Litter C input to the soil pools of the 3-pool-model for natural vegetation"),
              min          = 0,
              isocountries = FALSE))
}
