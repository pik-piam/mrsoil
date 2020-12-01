#' @title calcTillageArea
#' @description This function calculates the tillage area shares for different tillage types
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#' @param tillage 'historicNoTill' (default) or 'ruleBased'; 'histroricNoTill' is based on the same assunmptions as 'ruleBased',
#'                 but additionally accounts for historic data on no tillage areas based on Porwolliks datasets.
#'                 'ruleBased' assumes full tillage for annual crops, and reduced tillage for perennials.
#' @examples
#' \dontrun{ calcOutput("TillageArea", aggregate = FALSE) }
#'
#'@importFrom magpiesets findset

calcTillageArea <- function(tillage="historicNoTill") {

  Cropland            <- toolFillYears(calcOutput("Croparea", cellular=TRUE, aggregate=FALSE), sort(findset("past_soc")))

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

  mapping            <- as.data.frame(list(crop2tillage, names(crop2tillage)), col.names =  c("crop","tillage"))
  cell.till_areaShr  <- toolAggregate(Cropland, rel=mapping, from="crop", to="tillage", partrel=TRUE, dim=3)/dimSums(Cropland, dim=3)
  cell.till_areaShr  <- toolConditionalReplace(cell.till_areaShr, conditions = c("is.na()"), replaceby = 0)

  param.till <- c("fulltill", "reducedtill", "notill")

  if(length(missingTillage <- setdiff(param.till, getItems(cell.till_areaShr, dim=3))) != 0){
    cell.till_areaShr <- add_columns(cell.till_areaShr, addnm=missingTillage, dim=3.1)
    cell.till_areaShr[,,missingTillage] <- 0
  }

  if(tillage == "historicNoTill"){

    noTillAreas <- readSource("PorwolliksGriddedTillage", convert="onlycorrect")
    noTillAreas <- toolFillYears(noTillAreas, getYears(cell.till_areaShr))

    cell.till_areaShr[,,"notill"][noTillAreas==1]              <- 1
    cell.till_areaShr[,,"reducedtill"][noTillAreas==1]         <- 0
    cell.till_areaShr[,,"fulltill"][noTillAreas==1]            <- 0

  } else if(tillage!="ruleBased") {stop("tillage setting is unknown. Please use: 'ruleBased' or 'historicNoTill'.")}

  return(list(
    x= cell.till_areaShr,
    weight=NULL,
    unit="",
    description="Tillage area shares (unitless)",
    isocountries=FALSE))
}
