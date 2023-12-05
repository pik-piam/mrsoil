#' @title calcDecayRaw
#' @description This function wraps together the decay rate for all SOC sub-pool per year
#'              for mineral soils using the steady-state method (Tier 2) of the 2019
#'              Refinement to the 2006 IPP Guidelines for National Greenhouse Gas Inventories
#'
#' @param lpjmlNatveg Switch between LPJmL natveg versionstop
#' @param climatetype Switch between different climate scenarios
#' @param mode        "historicalSpinup" for historical period and
#'                    "magpieInput" for future
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#' @examples
#' \dontrun{
#' calcOutput("Decay", aggregate = FALSE)
#' }
#'
#' @import madrat
#' @import magclass

calcDecayRaw <- function(lpjmlNatveg = "LPJmL4_for_MAgPIE_44ac93de",
                         climatetype = "GSWP3-W5E5:historical",
                         mode        = "historicalSpinup") {

  param <- readSource("IPCCSoil", convert = FALSE)
  # kfaca = decay rate under optimum condition for active (k3)
  # kfacs = decay rate under optimum condition for slow (k4)
  # kfacp = decay rate under optimum condition for passive (k5)
  # k3par1 = sand intercept
  # k3par2 = sand slope

  .clean <- function(x, name) {
    return(add_dimension(collapseNames(x), dim = 3.2, add = "sub", nm = name))
  }

  .bundle <- function(funcName, staticSet, switchType, switchSet) {
    out <- NULL
    for (i in seq_along(switchType)) {

      tmp <- .clean(do.call("calcOutput", c(type = funcName, staticSet, switchSet[i])),
                    switchType[i])
      # check years and dims
      if (!is.null(out) && (is.null(getYears(out)) || is.null(getYears(tmp)))) {
        tmp <- setNames(magpie_expand(tmp, out), getNames(tmp))
        out <- setNames(magpie_expand(out, tmp), getNames(out))
      }
      years <- intersect(getYears(tmp), getYears(out))
      out   <- mbind(out[, years, ], tmp[, years, ])
    }
    return(out)
  }

  if (mode == "historicalSpinup") {

    cellWfactor <- .bundle(funcName   = "WaterEffectDecomposition",
                           switchType = c("crop",                    "natveg"),
                           switchSet  = c(irrigation = "mixedirrig", irrigation = "rainfed"),
                           staticSet  = list(lpjmlNatveg = lpjmlNatveg, climatetype = climatetype, aggregate = FALSE))

    cellTillFactor <- .bundle(funcName   = "TillageEffectDecomposition",
                              switchType = c("crop",               "natveg"),
                              switchSet  = c(tillage = "default", tillage = "notill"),
                              staticSet  = list(aggregate = FALSE))

  } else if (mode == "magpieInput") {

    cellWfactor <- .bundle(funcName   = "WaterEffectDecomposition",
                           switchType = c("rainfed",                "irrigated"),
                           switchSet  = c(irrigation = "rainfed", irrigation = "irrigated"),
                           staticSet  = list(lpjmlNatveg = lpjmlNatveg, climatetype = climatetype, aggregate = FALSE))

    cellTillFactor <- .bundle(funcName   = "TillageEffectDecomposition",
                              switchType = c("notill", "reducedtill", "fulltill"),
                              switchSet  = c(tillage = "notill",
                                             tillage = "reducedtill",
                                             tillage = "fulltill"),
                              staticSet  = list(aggregate = FALSE))
  } else {

    stop("Mode setting unknown. Possible types: 'magpieInput' or 'historicalSpinup'") # add message here
  }

  cellTempFactor <- calcOutput("TempEffectDecomposition",
                               lpjmlNatveg = lpjmlNatveg, climatetype = climatetype, aggregate = FALSE)
  cellSandFrac   <- calcOutput("SandFrac", aggregate = FALSE)

  activeDecay    <-  param[, , "kfaca"] * cellWfactor * cellTempFactor * cellTillFactor *
    (param[, , "k3par1"] +  param[, , "k3par2"] * cellSandFrac)
  slowDecay      <- param[, , "kfacs"] * cellWfactor * cellTempFactor * cellTillFactor
  passiveDecay   <- param[, , "kfacp"] * cellWfactor * cellTempFactor

  decay <- magpiesort(mbind(.clean(activeDecay,  "active"),
                            .clean(slowDecay,    "slow"),
                            .clean(passiveDecay, "passive")))

  return(list(x      = decay,
              weight = NULL,
              unit   = "per yr",
              description  = "Decay rate for all SOC sub-pool per year",
              isocountries = FALSE))
}
