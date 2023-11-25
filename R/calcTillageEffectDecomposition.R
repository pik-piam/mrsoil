#' @title calcTillageEffectDecomposition
#' @description This function calculates the tillage effect on decomposition
#'              for mineral soils using the steady-state method (Tier 2) of the 2019
#'              Refinement to the 2006 IPP Guidelines for National Greenhouse Gas Inventories
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#' @param tillage tillage type to de considered.
#'                'histtill' is historic tillage area shares based on no
#'                tillage areas from Porwollik together with rule based assumption;
#'                'mixedtill' includes pure rule based assumptions.
#'                Other options: 'fulltill', 'notill', 'reducedtill', 'default'
#'
#' @examples
#' \dontrun{
#'   calcOutput("TillageEffectDecomposition", aggregate = FALSE)
#' }

calcTillageEffectDecomposition <- function(tillage = "default") {

  default <- "mixedtill" #to be better align with magpie simulations as long as not till is not included
  tillage <- ifelse(tillage == "default", default, tillage)

  paramTill     <- readSource("IPCCSoil", convert = FALSE)[, , "tillfac", pmatch = TRUE]
  tillage2param <- c(fulltill    = "tillfac_ft",
                     reducedtill = "tillfac_rt",
                     notill      = "tillfac_nt")

  if (tillage %in% c("fulltill", "reducedtill", "notill")) {

    cellTillFactor    <- setNames(paramTill[, , tillage2param[tillage]],
                                  names(tillage2param[tillage]))

  } else if (tillage %in% c("mixedtill", "histtill")) {

    cellTillAreaShr  <- calcOutput("TillageArea", tillage = tillage, aggregate = FALSE)
    paramTill        <- setNames(paramTill[, , tillage2param], names(tillage2param))
    cellTillFactor   <- setNames(dimSums(paramTill * cellTillAreaShr, dim = 3), tillage)

  } else {
    stop("tillage setting is unknown. Please use: 'mixedtill', 'histtill', 'fulltill',
         'reducedtill' or 'notill'.")
  }

  return(list(x            = cellTillFactor,
              weight       = NULL,
              unit         = "",
              description  = "Tillage effect on decomposition for mineral soils (unitless)",
              isocountries = FALSE))
}
