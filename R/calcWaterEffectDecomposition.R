#' @title calcWaterEffectDecomposition
#' @description This function calculates the water effect on decomposition
#'              for mineral soils using the steady-state method (Tier 2) of the 2019
#'              Refinement to the 2006 IPP Guidelines for National Greenhouse Gas Inventories
#'              for rainfed and irrigated systems
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#' @param irrigation  irrigation type to de considered. Default (mixedirrig) is historic
#'                    irrigation area shares to calculate area weighted mean over rainfed
#'                    and irrigated factors. Other options: rainfed, irrigated
#' @param lpjml       Switch between LPJmL natveg versionstop
#' @param climatetype Switch between different climate scenarios
#'
#' @examples
#' \dontrun{
#' calcOutput("WaterEffectDecomposition", aggregate = FALSE)
#' }
#'
#' @importFrom magpiesets findset

calcWaterEffectDecomposition <- function(irrigation  = "mixedirrig",
                                         lpjml       = "LPJmL4_for_MAgPIE_44ac93de",
                                         climatetype = "GSWP3-W5E5:historical") {

  stage <- ifelse(grepl("historical", climatetype),
                  yes = "raw1901",
                  no  = "raw")

  if (irrigation == "rainfed") {

    param           <- readSource("IPCCSoil", convert = FALSE)
    paramWintercept <- setYears(param[, , "wfacpar1"], NULL)
    paramWslope     <- setYears(param[, , "wfacpar2"], NULL)
    paramWslope2    <- setYears(param[, , "wfacpar3"], NULL)

    cellPrep   <- calcOutput("LPJmLClimateInput", climatetype  = climatetype,
                             variable     = "precipitation:monthlySum",
                             stage        = stage,
                             lpjmlVersion = lpjml,
                             aggregate    = FALSE)

    cellPet    <- calcOutput(type = "LPJmL_new", climatetype = climatetype,
                             subtype   = "mpet",
                             stage     = stage,
                             version   = lpjml,
                             aggregate = FALSE)

    years      <- intersect(getYears(cellPet), getYears(cellPrep))

    cellMappet                    <- cellPrep[, years, ] / cellPet[, years, ]
    cellMappet[cellMappet > 1.25] <- 1.25
    cellMappet                    <- toolConditionalReplace(cellMappet, "is.na()", 0)

    cellWmonthFactor  <- paramWintercept + paramWslope  * cellMappet -
                           paramWslope2 * cellMappet**2
    cellWmonthFactor  <- add_dimension(collapseNames(cellWmonthFactor),
                                       dim = 3.1, add = "irrigation", nm = "rainfed")

    cellWfactor       <- 1.5 * dimSums(cellWmonthFactor, dim = 3.2) / 12

  } else if (irrigation == "irrigated") {
    # irrigation for every month of the year
    # improve later by using just growing month
    # days_per_month <- calcOutput("GrowingPeriod", aggregate = FALSE)???

    map   <- toolGetMappingCoord2Country()
    cells <- paste(map$coords, map$iso, sep = ".")

    cellWmonthFactor <- new.magpie(cells, NULL, c("jan", "feb", "mar", "apr", "mai", "jun",
                                                  "jul", "aug", "sep", "oct", "nov", "dec"))
    cellWmonthFactor <- add_dimension(collapseNames(cellWmonthFactor), dim = 3.1,
                                      add = "irrigation", nm = "irrigated")
    cellWmonthFactor[, , "irrigated"] <- 0.775

    cellWfactor       <- 1.5 * dimSums(cellWmonthFactor, dim = 3.2) / 12

  } else if (irrigation == "mixedirrig") {

    if (!grepl("historical", climatetype)) {
      stop("'mixedirrig' is only supported for the historical period")
    }

    cellIrAreaShr  <- readSource("LUH2v2", subtype = "irrigation", convert = "onlycorrect")
    cellIrAreaShr  <- dimSums(cellIrAreaShr, dim = 3)

    cellWrainfed   <- calcOutput("WaterEffectDecomposition",
                                         climatetype = climatetype, lpjml = lpjml,
                                         irrigation = "rainfed",  aggregate = FALSE)
    cellWirrigated <- calcOutput("WaterEffectDecomposition",
                                         climatetype = climatetype, lpjml = lpjml,
                                         irrigation = "irrigated", aggregate = FALSE)

    years          <- intersect(getYears(cellIrAreaShr), getYears(cellWrainfed))

    cellWfactor    <- setNames(cellWirrigated * cellIrAreaShr[, years, ], "mixed") +
                        setNames(cellWrainfed[, years, ] * (1 - cellIrAreaShr[, years, ]), "mixed")

  } else {
    stop("Irrigation setting is unknown. Please use: 'mixedirrig','rainfed' or 'irrigated'.")
  }

  return(list(x            = cellWfactor,
              weight       = NULL, # only cellular level supported
              unit         = "",
              description  = "Water effect on decomposition for mineral soils (unitless)",
              isocountries = FALSE))
}
