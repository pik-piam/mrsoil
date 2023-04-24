#' @title calcTillageEffectDecomposition
#' @description This function calculates the tillage effect on decomposition
#'              for mineral soils using the steady-state method (Tier 2) of the 2019
#'              Refinement to the 2006 IPP Guidelines for National Greenhouse Gas Inventories
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#' @param tillage tillage type to de considered.
#'                'histtill' (default) is historic tillage area shares based on no
#'                tillage areas from Porwollik together with rule based assumption;
#'                'mixedtill' includes pure rule based assumptions.
#'                Other options: 'fulltill', 'notill', 'reducedtill'
#'
#' @examples
#' \dontrun{
#'   calcOutput("TillageEffectDecomposition", aggregate = FALSE)
#' }

calcTillageEffectDecomposition <- function(tillage = "histtill") {

  paramTill     <- readSource("IPCCSoil", convert = FALSE)[, , "tillfac", pmatch = TRUE]
  tillage2param <- c(fulltill    = "tillfac_ft",
                     reducedtill = "tillfac_rt",
                     notill      = "tillfac_nt")

  if (tillage %in% c("fulltill", "reducedtill", "notill")) {

    cellTillFactor    <- setNames(paramTill[, , tillage2param[tillage]],
                                  names(tillage2param[tillage]))

  } else if (tillage %in% c("mixedtill", "histtill")) {

    tillage2area     <- c(mixedtill   = "ruleBased",
                          histtill    = "historicNoTill")

    cellTillAreaShr  <- calcOutput("TillageArea", tillage = tillage2area[tillage], aggregate = FALSE)
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
