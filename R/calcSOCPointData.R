#' @title calcSOCPointData
#' @description This function return SOC point data for validation from Sanderman et al., 2017
#'
#' @return magpie object in cellular resolution
#' @author Kristine Karstens
#'
#' @seealso
#'  \code{\link[mrsoil]{readSanderman}}
#'
#' @examples
#'  \dontrun{ calcOutput("calcSOCPointData", aggregate = FALSE) }

calcSOCPointData <- function() {

  x <- readSource("Sanderman")

  return(list(
    x            = x,
    weight       = 0,
    unit         = "tons C per ha",
    description  = "SOC point data for validation from Sanderman et al., 2017",
    isocountries = FALSE))
}
