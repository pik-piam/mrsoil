#' @title readTillage
#' @description Read historical tillage data set based on (https://www.earth-syst-sci-data.net/11/823/2019/)
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author Kristine Karstens
#' @seealso
#' \code{\link{readLPJmL}},
#' \code{\link{read.LPJ_input}}
#' @examples
#'
#' \dontrun{
#'   readSource("Tillage")
#' }
#'
#' @import madrat
#' @import magclass
#' @importFrom lpjclass read.LPJ_input

readTillage <- function(){

  file   <- grep("tillage", list.files("."), value=TRUE)
  years  <- unlist(regmatches(file, gregexpr("\\d{4}",file)))
  x      <- read.LPJ_input(file_name=file, out_years=paste0("y",years[1]:years[2]), four_d = TRUE)
  x    <- as.magpie(x)
  getNames(x) <- "tillage"

  return(x)
}
