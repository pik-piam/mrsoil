#' @title toolBindSubtypes
#' @description This function mbinds subtypes for different functions
#'
#' @param subtype_name subtype name that should be binded
#' @param subtype_list subtype list that should be binded
#' @param type calcOutput-type
#' @param ... addition arguments handed over to calcOutput call
#'
#' @return magpie object with global parameters
#' @author Kristine Karstens
#'
#' @seealso
#' \code{\link{readIPCCSoil}}
#'
#' @examples
#' \dontrun{ downloadSource("IPCCSoil") }
#'
#' @import madrat
#' @import magclass
#' @importFrom utils download.file tail
#' @importFrom madrat toolSubtypeSelect

toolBindSubtypes <- function(subtype_name, subtype_list, type, ...) {

  out <- NULL

  for(item in subtype_list){
    out <- mbind(out, calcOutput(type=type, eval(parse(text=paste0(subtype_name,"= '",item,"'"))), ...))
  }

  return(out)
}
