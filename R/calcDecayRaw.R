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

  .clean <- function(x, name, dimname) {
    return(add_dimension(collapseNames(x), dim = 3.1, add = dimname, nm = name))
  }

  .bundle <- function(funcName, staticSet, switchType, switchSet, dimname) {
    out <- NULL
    for (i in seq_along(switchType)) {

      tmp <- .clean(do.call("calcOutput", c(type = funcName, staticSet, switchSet[i])),
                    name = switchType[i], dimname = dimname)
      # check years and dims
      if (!is.null(out) && (is.null(getYears(out)) || is.null(getYears(tmp)))) {
        tmp <- magpie_expand(tmp, collapseDim(out[, , 1], dim = 3))
        out <- magpie_expand(out, collapseDim(tmp, dim =3))
      }

      years <- intersect(getYears(tmp), getYears(out))
      if(!is.null(years)){
        out <- out[, years, ]
        tmp <- tmp[, years, ]
      }
      out <- mbind(out, tmp)
    }
    return(out)
  }

  if (mode == "historicalSpinup") {

    cellWfactor <- .bundle(funcName   = "WaterEffectDecomposition",
                           switchType = c("crop",                    "natveg"),
                           switchSet  = c(irrigation = "mixedirrig", irrigation = "rainfed"),
                           staticSet  = list(lpjmlNatveg = lpjmlNatveg, climatetype = climatetype, aggregate = FALSE),
                           dimname    = "lutype")

    cellTillFactor <- .bundle(funcName   = "TillageEffectDecomposition",
                              switchType = c("crop",               "natveg"),
                              switchSet  = c(tillage = "default", tillage = "notill"),
                              staticSet  = list(aggregate = FALSE),
                              dimname    = "lutype")

  } else if (mode == "magpieInput") {

    cellWfactor <- .bundle(funcName   = "WaterEffectDecomposition",
                           switchType = c("rainfed",                "irrigated"),
                           switchSet  = c(irrigation = "rainfed", irrigation = "irrigated"),
                           staticSet  = list(lpjmlNatveg = lpjmlNatveg, climatetype = climatetype, aggregate = FALSE),
                           dimname    = "irrigation")

    cellTillFactor <- .bundle(funcName   = "TillageEffectDecomposition",
                              switchType = c("notill", "reducedtill", "fulltill"),
                              switchSet  = c(tillage = "notill",
                                             tillage = "reducedtill",
                                             tillage = "fulltill"),
                              staticSet  = list(aggregate = FALSE),
                              dimname    = "tillage")
  } else {

    stop("Mode setting unknown. Possible types: 'magpieInput' or 'historicalSpinup'") # add message here
  }

  cellTempFactor <- calcOutput("TempEffectDecomposition",
                               lpjmlNatveg = lpjmlNatveg, climatetype = climatetype, aggregate = FALSE)
  cellSandFrac   <- calcOutput("SandFrac", aggregate = FALSE)

  # harmonize years
  .getCommonYears <- function(listOfYearVectors){
    nullIndex         <- which(sapply(listOfYearVectors, is.null))
    if(length(nullIndex) != 0) {
      vcat(1, "There are objects with no years (NULL) provided.")
      listOfYearVectors <- listOfYearVectors[-nullIndex]
    }
    commonYears       <- Reduce(intersect, listOfYearVectors)
    if(length(commonYears) == 0) vcat(0, "There are no common years objects provided.")
    return(commonYears)
  }

  years <- .getCommonYears(list(getYears(cellTempFactor),
                                getYears(cellTillFactor),
                                getYears(cellWfactor)))

  if(!is.null(getYears(cellTempFactor))) cellTempFactor <- cellTempFactor[, years, ]
  if(!is.null(getYears(cellTillFactor))) cellTillFactor <- cellTillFactor[, years, ]
  if(!is.null(getYears(cellWfactor)))    cellWfactor    <- cellWfactor[, years, ]

  activeDecay    <-  param[, , "kfaca"] * cellWfactor * cellTempFactor * cellTillFactor *
    (param[, , "k3par1"] +  param[, , "k3par2"] * cellSandFrac)
  slowDecay      <- param[, , "kfacs"] * cellWfactor * cellTempFactor * cellTillFactor
  passiveDecay   <- param[, , "kfacp"] * cellWfactor * cellTempFactor

  activeDecay  <- .clean(activeDecay,  "active",  "sub")
  slowDecay    <- .clean(slowDecay,    "slow",    "sub")
  passiveDecay <- .clean(passiveDecay, "passive", "sub")

  if(length(getNames(passiveDecay)) != length(getNames(activeDecay))) {
    passiveDecay <- dimOrder(magpie_expand(passiveDecay,
                                           collapseDim(activeDecay[, , "rainfed"], keepdim = 3.3)),
                             perm = c(2,3,1), dim = 3)
  }
  decay <- magpiesort(mbind(activeDecay, slowDecay, passiveDecay))

  return(list(x      = decay,
              weight = NULL,
              unit   = "per yr",
              description  = "Decay rate for all SOC sub-pool per year",
              isocountries = FALSE))
}
