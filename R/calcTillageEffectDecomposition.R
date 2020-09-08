#' @title calcTillageEffectDecomposition
#' @description This function calculates the tillage effect on decomposition for mineral soils
#' using the steady-state method (Tier 2) of the 2019 Refinement to the 2006 IPP Guidelines
#' for National Greenhouse Gas Inventories
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#' @param tillage tillage type to de considered.
#'                   Default (histill) is historic tillage area shares based on no tillage areas from Porwollik together with rule based assumption;
#'                   'mixedtill' includes pure rule based assumptions. Other options: fulltill, notill, reducedtill
#'
#' @examples
#' \dontrun{ calcOutput("TillageEffectDecomposition", aggregate = FALSE) }
#'
#' @import madrat
#' @import magclass
#' @import mrcommons

calcTillageEffectDecomposition <- function(tillage="histtill") {

  param.till <- readSource("IPCCSoil", convert=FALSE)[,,"tillfac", pmatch=TRUE]

  tillage2param       <- c(fulltill    = "tillfac_ft",
                           reducedtill = "tillfac_rt",
                           notill      = "tillfac_nt")

  if(tillage %in% c("fulltill", "reducedtill", "notill")){

    cell.till_Factor    <- setNames(param.till[,,tillage2param[tillage]], names(tillage2param[tillage]))

  } else if(tillage %in% c("mixedtill", "histtill")){

    tillage2area       <- c(mixedtill    = "ruleBased",
                             histtill    = "historicNoTill")

    cell.till_areaShr  <- calcOutput("TillageArea", tillage=tillage2area[tillage], aggregate=FALSE)
    param.till         <- setNames(param.till[,,tillage2param], names(tillage2param))

    cell.till_Factor   <- setNames(dimSums(param.till * cell.till_areaShr, dim=3), tillage)

  } else {stop("tillage setting is unknown. Please use: 'mixedtill','fulltill', 'reducedtill' or 'notill'.")}

  return(list(
    x=cell.till_Factor,
    weight=NULL,
    unit="",
    description="Tillage effect on decomposition for mineral soils (unitless)",
    isocountries=FALSE))
}
