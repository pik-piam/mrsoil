#' @title calcWaterEffectDecomposition
#' @description This function calculates the water effect on decomposition for mineral soils
#' using the steady-state method (Tier 2) of the 2019 Refinement to the 2006 IPP Guidelines
#' for National Greenhouse Gas Inventories for rainfed and irrigated systems
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#' @param irrigation irrigation type to de considered. Default (mixedirrig) is historic irrigation area shares
#'                   to calculate area weighted mean over rainfed and irrigated factors. Other options: rainfed, irrigated
#' @param climate_scen climate configuration
#' @examples
#' \dontrun{ calcOutput("WaterEffectDecomposition", aggregate = FALSE) }
#'
#' @import madrat
#' @import magclass
#' @import mrcommons
#' @importFrom magpiesets findset

calcWaterEffectDecomposition <- function(irrigation="mixedirrig", climate_scen="default") {

  if(irrigation=="rainfed"){

    param <- readSource("IPCCSoil", convert=FALSE)
    param.w_intercept <- setYears(param[,,"wfacpar1"], NULL)
    param.w_slope     <- setYears(param[,,"wfacpar2"], NULL)
    param.w_slope2    <- setYears(param[,,"wfacpar3"], NULL)

    cell.prep   <- readSource("CRU", subtype="precipitation", convert = "onlycorrect")[,sort(findset("past_soc")),]
    cell.pet    <- readSource("CRU", subtype="potential_evap", convert = "onlycorrect")[,sort(findset("past_soc")),]

    if(grepl("freeze", climate_scen)){
      freeze_year <- as.integer(gsub("freeze","", climate_scen))
      reset_years <- getYears(cell.prep, as.integer=TRUE) >= freeze_year
      cell.prep[,reset_years,] <- setYears(cell.prep[,rep(freeze_year,sum(reset_years)),], getYears(cell.prep[,reset_years,]))
      cell.pet[,reset_years,]  <- setYears(cell.pet[,rep(freeze_year,sum(reset_years)),],  getYears(cell.pet[,reset_years,]))
    }

    cell.mappet                   <- cell.prep/cell.pet
    cell.mappet[cell.mappet>1.25] <- 1.25
    cell.mappet                   <- toolConditionalReplace(cell.mappet, "is.na()", 0)

    cell.w_monthFactor  <- param.w_intercept + param.w_slope  * cell.mappet - param.w_slope2 * cell.mappet**2
    cell.w_monthFactor  <- add_dimension(collapseNames(cell.w_monthFactor), dim=3.1, add="irrigation", nm="rainfed")

    cell.w_Factor       <- 1.5 * dimSums(cell.w_monthFactor, dim=3.2)/12

  } else if(irrigation=="irrigated"){

    # irrigation for every month of the year
    # improve later by using just growing month
    # days_per_month <- calcOutput("GrowingPeriod", aggregate = FALSE)

    years <- sort(findset("past_soc"))
    cells <- rownames(magpie_coord)
    cell.w_monthFactor <- new.magpie(cells, years, c("jan","feb","mar","apr","mai","jun","jul","aug","sep","oct","nov","dec"))
    cell.w_monthFactor <- add_dimension(collapseNames(cell.w_monthFactor), dim=3.1, add="irrigation", nm="irrigated")
    cell.w_monthFactor[,,"irrigated"] <- 0.775

    cell.w_Factor       <- 1.5 * dimSums(cell.w_monthFactor, dim=3.2)/12

  } else if(irrigation=="mixedirrig") {

    cell.ir_areaShr    <- toolCoord2Isocell(readSource("LUH2v2", subtype="irrigation", convert="onlycorrect"))[,sort(findset("past_soc")),]
    cell.ir_areaShr    <- dimSums(cell.ir_areaShr, dim=3)

    cell.w_Factor      <- mbind(calcOutput("WaterEffectDecomposition", irrigation="rainfed",   climate_scen=climate_scen, aggregate = FALSE),
                                calcOutput("WaterEffectDecomposition", irrigation="irrigated", climate_scen=climate_scen, aggregate = FALSE))

    cell.w_Factor      <- setNames(cell.w_Factor[,,"irrigated"] * cell.ir_areaShr, "mixed") +
                          setNames(cell.w_Factor[,,"rainfed"] * (1 - cell.ir_areaShr), "mixed")

  } else {stop("Irrigation setting is unknown. Please use: 'mixedirrig','rainfed' or 'irrigated'.")}

  return(list(
    x=cell.w_Factor,
    weight=NULL,
    unit="",
    description="Water effect on decomposition for mineral soils (unitless)",
    isocountries=FALSE))
}
