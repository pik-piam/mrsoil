#' @title readBrovkin
#' @description Read parameter files from Brovkin Paper (doi:10.5194/bg-9-565-2012)
#'
#' @return List of magpie objects with results on global level
#'
#' @author Kristine Karstens
#' @examples
#' \dontrun{
#'   readSource("Brovkin", convert = FALSE)
#' }
#'
#' @importFrom utils read.csv

readBrovkin <- function(){

  out <- as.magpie(read.csv("table1_litter_decomposition_parameters.csv"))

  return(out)
}
