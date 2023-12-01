#' @title calcSteadyState
#' @description This function wraps together the steady state for all sub-pool SOC stock for mineral soils
#'              using the steady-state method (Tier 2) of the 2019 Refinement to the 2006 IPP Guidelines
#'              for National Greenhouse Gas Inventories
#'
#' @param lpjmlNatveg Switch between LPJmL natveg versionstop
#' @param climatetype Switch between different climate scenarios
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#' @examples
#' \dontrun{
#' calcOutput("SteadyState", aggregate = FALSE)
#' }
#'

calcSteadyState <- function(lpjmlNatveg = "LPJmL4_for_MAgPIE_44ac93de",
                            climatetype = "GSWP3-W5E5:historical") {

  ###############################################################
  ######## Load carbon input based on InputMultiplier ###########

  # Load soil parameter collection
  soilParam          <- readSource("IPCCSoil", convert = FALSE)

  # Load fractional transfer coefficients for active to slow (sand fraction depending)
  f4act2slo      <- calcOutput("TransferActive2Slow", aggregate = FALSE)

  # Load fractional transfer coefficient for structual input component to active pool (tillage depending)
  tillage2param  <- c(fulltill    = "f2_ft",
                      reducedtill = "f2_rt",
                      notill      = "f2_nt")
  f2              <- setNames(soilParam[, , tillage2param], names(tillage2param))
  ### stabilization efficiencies for structural decay products entering the active pool
  ### for cropland are calculated using tillage area information
  tillAreaShr     <- calcOutput("TillageArea", aggregate = FALSE)
  f2struc2actCrop <- dimSums(f2 * tillAreaShr, dim = 3)
  ### backup: set all cell with no area info to full tillage (just in case it is needed)
  f2struc2actCrop[f2struc2actCrop == 0] <- f2[, , "fulltill"]
  f2struc2actCrop       <- setNames(f2struc2actCrop, "crop")
  ### stabilization efficiencies for structural decay products entering the active pool
  ### for natural vegetation is no tillage
  f2struc2actNatveg       <- setNames(f2struc2actCrop, "natveg")
  f2struc2actNatveg[, , ] <- f2[, , "notill"]
  f2struc2act <- mbind(f2struc2actCrop, f2struc2actNatveg)

  # Load carbon input from different sources
  .prep <- function(x, landtype, ...) {
    return(toolFillYears(add_dimension(x, dim = 3.3, nm = landtype, add = "landtype"),
                         sort(findset("past_soc"))))
  }

  residues   <- .prep(calcOutput("CarbonResidues", aggregate = FALSE), "crop")
  manure     <- .prep(calcOutput("CarbonManure",   aggregate = FALSE), "crop")
  litter     <- .prep(calcOutput("CarbonLitter",   lpjmlNatveg = lpjmlNatveg, climatetype = climatetype,
                                 mode = "historicalSpinup", fixfpc = TRUE, aggregate = FALSE), "natveg")
  cellInput  <- mbind(residues[, , "c"],           manure[, , "c"],           litter[, , "c"])
  inputProp  <- mbind(residues[, , c("LC", "NC")], manure[, , c("LC", "NC")], litter[, , c("LC", "NC")])
  rm(litter, manure, residues)

  ### NOT FINSIHED YET HERE!!!!
  alpha <- dimSums(toolCarbonInputMultiplier(inputProp = inputProp,
                                     soilParam = soilParam,
                                     f4act2slo = f4act2slo,
                                     f2struc2act = f2struc2act) * cellInput, dim = 3.2) #CLARIFY


  ###############################################################
  ######## Load decay rates & calc steaty states ################

  decay     <- calcOutput("DecayRaw", lpjmlNatveg = lpjmlNatveg, climatetype = climatetype,
                          mode = "historicalSpinup", aggregate = FALSE)

  .steadystate <- function(alpha, decay, name) {
    x <- toolConditionalReplace(alpha / decay[, , name], "is.na()", 0)
    return(add_dimension(collapseNames(x), dim = 3.2, add = "subpool", nm = name))
  }

  activeSteadyState  <- .steadystate(alpha[,,"active"],  decay, "active")
  slowSteadyState    <- .steadystate(alpha[,,"slow"],    decay, "slow")
  passiveSteadyState <- .steadystate(alpha[,,"passive"], decay, "passive")
  soilCarbonSteadyState <- magpiesort(mbind(activeSteadyState,
                                            slowSteadyState,
                                            passiveSteadyState))
  ###############################################################

  return(list(x      = soilCarbonSteadyState,
              weight = NULL,
              unit   = "per yr",
              description  = "Steady-state for all SOC sub-pool per year",
              isocountries = FALSE))
}
