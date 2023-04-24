#' @title calcTillageArea
#' @description This function calculates the tillage area shares for different tillage types
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#' @param tillage 'historicNoTill' (default) or 'ruleBased'; histroricNoTill' is based
#'                on the same assunmptions as 'ruleBased', but additionally accounts for
#'                historic data on no tillage areas based on Porwolliks datasets.
#'                'ruleBased' assumes full tillage for annual crops, and reduced tillage for perennials.
#' @examples
#' \dontrun{
#' calcOutput("TillageArea", aggregate = FALSE)
#' }
#'
#' @importFrom magpiesets findset

calcTillageArea <- function(tillage = "historicNoTill") {

  cropland            <- toolFillYears(calcOutput("Croparea", cellular = TRUE, aggregate = FALSE),
                                       sort(findset("past_soc")))

  crop2tillage        <- c(fulltill = "cassav_sp",
                           fulltill = "cottn_pro",
                           fulltill = "foddr",
                           fulltill = "groundnut",
                           fulltill = "maiz",
                           fulltill = "potato",
                           fulltill = "puls_pro",
                           fulltill = "rapeseed",
                           fulltill = "rice_pro",
                           fulltill = "soybean",
                           fulltill = "sugr_beet",
                           fulltill = "sunflower",
                           fulltill = "tece",
                           fulltill = "trce",
                           reducedtill = "sugr_cane",
                           reducedtill = "oilpalm",
                           reducedtill = "others")

  mapping         <- as.data.frame(list(crop2tillage, names(crop2tillage)),
                                   col.names =  c("crop", "tillage"))
  cellTillAreaShr <- toolAggregate(cropland, rel = mapping, from = "crop", to = "tillage",
                                   partrel = TRUE, dim = 3) / dimSums(cropland, dim = 3)
  cellTillAreaShr <- toolConditionalReplace(cellTillAreaShr, conditions = c("is.na()"), replaceby = 0)

  paramTill <- c("fulltill", "reducedtill", "notill")

  if (length(missingTillage <- setdiff(paramTill, getItems(cellTillAreaShr, dim = 3))) != 0) {
    cellTillAreaShr <- add_columns(cellTillAreaShr, addnm = missingTillage, dim = 3.1)
    cellTillAreaShr[, , missingTillage] <- 0
  }

  if (tillage == "historicNoTill") {

    noTillAreas <- readSource("PorwolliksGriddedTillage", convert = "onlycorrect")
    noTillAreas <- toolFillYears(noTillAreas, getYears(cellTillAreaShr))

    cellTillAreaShr[, , "notill"][noTillAreas == 1]              <- 1
    cellTillAreaShr[, , "reducedtill"][noTillAreas == 1]         <- 0
    cellTillAreaShr[, , "fulltill"][noTillAreas == 1]            <- 0

  } else if (tillage != "ruleBased") {
    stop("tillage setting is unknown. Please use: 'ruleBased' or 'historicNoTill'.")
  }

  return(list(x            = cellTillAreaShr,
              weight       = NULL,
              unit         = "",
              description  = "Tillage area shares (unitless)",
              isocountries = FALSE))
}
