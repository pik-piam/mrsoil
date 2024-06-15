#' @title calcSoilCarbon
#' @description Calculates SOC states
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#' @param output "all" (default), "actualstate", "carbontransfer", "interstate", "naturalstate"
#' @param lpjmlNatveg Switch between LPJmL natveg versionstop
#' @param climatetype Switch between different climate scenarios
#'
#' @examples
#' \dontrun{
#' calcOutput("SoilCarbon")
#' }
#' @importFrom magclass setNames
#' @importFrom magpiesets findset

calcSoilCarbon <- function(output      = "all",
                           lpjmlNatveg = "LPJmL4_for_MAgPIE_44ac93de",
                           climatetype = "GSWP3-W5E5:historical") {

  if (output ==  "all") {
    #######################
    ### Load Data & Ini ###
    #######################

    # Load Landuse data
    landuse               <- calcOutput("Landuse", aggregate = FALSE)
    soilCarbonSteadyState <- calcOutput("SteadyState", lpjmlNatveg = lpjmlNatveg,
                                        climatetype = climatetype, aggregate = FALSE)
    decay                 <- calcOutput("DecayRaw",    lpjmlNatveg = lpjmlNatveg,
                                        climatetype = climatetype, aggregate = FALSE)
    soilCarbonInit        <- calcOutput("SoilCarbonSpinup", aggregate = FALSE)

    ######################
    ### Carbon Cycling ###
    ######################

    out <- toolSoilCarbonCycling(soilCarbonInit,
                                 soilCarbonSteadyState,
                                 decay,
                                 landuse)


    getSets(out, fulldim = FALSE)[1] <- "x.y.iso"

  } else if (output %in% c("actualstate", "interstate", "naturalstate")) {

    out      <- calcOutput("SoilCarbon", aggregate = FALSE, output = "all",
                           lpjmlNatveg = lpjmlNatveg, climatetype = climatetype)[, , output]
    landuse  <- calcOutput("Landuse", aggregate = FALSE)[, getYears(out), ]
    out      <- out * landuse

  } else if (output == "carbontransfer") {

    out <- calcOutput("SoilCarbon", aggregate = FALSE, output = "all",
                      lpjmlNatveg = lpjmlNatveg, climatetype = climatetype)[, , output]

  } else {
    stop("'output' unknown.")
  }

  out <- collapseDim(out)

  return(list(x            = out,
              weight       = NULL,
              unit         = ifelse(output == "all", "tC per ha", "Mt C"),
              description  = "Carbon budget on croplands for historical period",
              isocountries = FALSE)
  )
}
