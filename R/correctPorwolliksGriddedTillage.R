#' @title correctPorwolliksGriddedTillage
#' @description Read historical tillage data set based on (https://www.earth-syst-sci-data.net/11/823/2019/)
#' @param x magpie object provided by the read function
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author Kristine Karstens
#' @seealso
#' \code{\link[mrlandcore]{readLPJmL_new}},
#' \code{\link[lpjclass]{read.LPJ_input}}
#' @examples
#' \dontrun{
#' readSource("PorwolliksGriddedTillage", convert = "onlycorrect")
#' }
#'
#' @import madrat
#' @import magclass
#' @importFrom lpjclass read.LPJ_input

correctPorwolliksGriddedTillage <- function(x) {

  x <- toolConditionalReplace(x, conditions = c("is.na()", "<0"), replaceby = 0)

  return(x)
}
