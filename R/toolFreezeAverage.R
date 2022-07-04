#' @title toolFreezeAverage
#' @description Calculates the 10 year average and freezes the effect for all years after
#'
#' @param x variable to freeze
#' @param freeze_year start of freeze period
#' @return magpie object wth froozen effect
#' @author Kristine Karstens
#'
#' @importFrom magclass getYears setYears
#' @importFrom madrat toolTimeAverage

toolFreezeAverage <- function(x, freeze_year){

  reset_years      <- getYears(x, as.integer=TRUE) > freeze_year
  avg              <- toolTimeAverage(x[,seq(freeze_year-5,freeze_year+5,1),], 11)
  x[,reset_years,] <- setYears(avg[,rep(1,sum(reset_years)),], getYears(x[,reset_years,]))

  return(x)
}
