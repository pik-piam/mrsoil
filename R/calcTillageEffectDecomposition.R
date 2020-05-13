#' @title calcTillageEffectDecomposition
#' @description This function calculates the tillage effect on decomposition for mineral soils
#' using the steady-state method (Tier 2) of the 2019 Refinement to the 2006 IPP Guidelines
#' for National Greenhouse Gas Inventories
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#' @param tillage tillage type to de considered. Default (mixedtill) is historic tillage area shares
#'                   to calculate area weighted mean over full, reduced and no tillage types
#'                   Other options: fulltill, notill, reducedtill
#' @examples
#' \dontrun{ calcOutput("TillageEffectDecomposition", aggregate = FALSE) }
#'
#' @import madrat
#' @import magclass
#' @import mrcommons

calcTillageEffectDecomposition <- function(tillage="mixedtill") {

  param.till <- readSource("IPCCSoil", convert=FALSE)[,,"tillfac", pmatch=TRUE]

  if(tillage %in% c("fulltill", "reducedtill", "notill")){

    tillage2param       <- c(fulltill   = "tillfac_ft",
                             reducedtil = "tillfac_rt",
                             notill     = "tillfac_nt")

    cell.till_Factor       <- setNames(param.till[,,tillage2param[tillage]], names(tillage2param[tillage]))

  } else if(tillage=="mixedtill"){

    Cropland            <- toolFillYears(calcOutput("Croparea", cellular=TRUE, aggregate=FALSE), sort(findset("past_all")))

    crop2tillage        <- c(tillfac_ft = "cassav_sp",
                             tillfac_ft = "cottn_pro",
                             tillfac_ft = "foddr",
                             tillfac_ft = "groundnut",
                             tillfac_ft = "maiz",
                             tillfac_ft = "potato",
                             tillfac_ft = "puls_pro",
                             tillfac_ft = "rapeseed",
                             tillfac_ft = "rice_pro",
                             tillfac_ft = "soybean",
                             tillfac_ft = "sugr_beet",
                             tillfac_ft = "sunflower",
                             tillfac_ft = "tece",
                             tillfac_ft = "trce",
                             tillfac_rt = "sugr_cane",
                             tillfac_rt = "oilpalm",
                             tillfac_rt = "others")

    mapping            <- as.data.frame(list(crop2tillage, names(crop2tillage)), col.names =  c("crop","tillage"))
    cell.till_areaShr  <- toolAggregate(Cropland, rel=mapping, from="crop", to="tillage", partrel=TRUE, dim=3)/dimSums(Cropland, dim=3)
    cell.till_areaShr  <- toolConditionalReplace(cell.till_areaShr, conditions = c("is.na()"), replaceby = 0)

    if(length(missingTillage <- setdiff(getItems(param.till, dim=3), getItems(cell.till_areaShr, dim=3))) != 0){
      cell.till_areaShr <- add_columns(cell.till_areaShr, addnm=missingTillage, dim=3)
      cell.till_areaShr[,,missingTillage] <- 0
    }

    cell.till_Factor   <- setNames(dimSums(param.till * cell.till_areaShr, dim=3),"mixedtill")

  } else {stop("tillage setting is unknown. Please use: 'mixedtill','fulltill', 'reducedtill' or 'notill'.")}

  return(list(
    x=cell.till_Factor,
    weight=NULL,
    unit="",
    description="Tillage effect on decomposition for mineral soils (unitless)",
    isocountries=FALSE))
}
