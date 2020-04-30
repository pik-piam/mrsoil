#' @title readIPCCSoil
#' @description Read IPCC Guideline tables
#' @param subtype Switch between different input
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author Kristine Karstens
#' @seealso
#' \code{\link{downloadIPCCSoil}},
#' @examples
#'
#' \dontrun{
#' readSource("IPCCSoil")
#' }
#'
#' @import madrat
#' @importFrom readxl read_excel
#' @importFrom utils read.csv

readIPCCSoil <- function(subtype="steady_state") {

  if(subtype=="steady_state"){

    data <- read_excel("19R_V4_Ch05_Tier2_Steady_State_Method-Spreadsheet_advance.xlsx", sheet="Model Parameters", skip =2)
    out  <- new.magpie(names = as.vector(data$Parmeter), sets = c("region", "year", "data"))
    out[,,as.vector(data$Parmeter)]  <- as.numeric(data$Default)

  }

  return(out)
}
