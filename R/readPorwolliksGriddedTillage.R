#' @title readPorwolliksGriddedTillage
#' @description Read historical tillage data set based on (https://www.earth-syst-sci-data.net/11/823/2019/)
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author Kristine Karstens
#' @seealso
#' \code{\link[mrcommons]{readLPJmL}},
#' \code{\link[lpjclass]{read.LPJ_input}}
#' @examples
#' \dontrun{
#' readSource("PorwolliksGriddedTillage")
#' }
#'
#' @import madrat
#' @import magclass
#' @importFrom magpiesets addLocation
#' @importFrom lpjclass read.LPJ_input

readPorwolliksGriddedTillage <- function() {

  file   <- grep("tillage", list.files("."), value = TRUE)
  years  <- unlist(regmatches(file, gregexpr("\\d{4}", file)))
  x      <- read.LPJ_input(file_name = file,
                           out_years = paste0("y", years[1]:years[2]),
                           ncells = 67420)
  class(x) <- "array"
  x        <- collapseNames(as.magpie(x, spatial = 1))
  x        <- collapseDim(addLocation(x), dim = c("N", "region"))
  x        <- clean_magpie(x)
  getNames(x) <- "tillage"

  # map no tillage -> 1 and tillage -> 0
  x <- 1 - x

  return(x)
}
