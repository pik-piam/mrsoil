#' @title readSOCbudgetParam
#' @description Read SOC budget parameters for sensitivity analysis
#' @param subtype Switch between different input
#'
#' @author Kristine Karstens
#'
#' @examples
#'
#' \dontrun{
#' readSource("readSOCbudgetParam")
#' }
#'
#' @importFrom utils read.table

readSOCbudgetParam <- function(subtype="default"){

  filename <- paste0(getOption("litter_param"),".csv")
  x <- as.magpie(read.table(filename, sep=",", header = TRUE))

  return(x)
}
