#' @title toolFreezeEffect
#' @description This function freeze values given a specific year and optionally given an additional side constrain
#'
#' @param x data set to freeze
#' @param year year to hold constant (onwards)
#' @param constrain if FALSE, no constrain. Other options: 'first_use' (freeze from 'first use' ( <=> !=0 ))
#'
#' @return magpie object with global parameters
#' @author Kristine Karstens


toolFreezeEffect <- function(x, year, constrain=FALSE) {

    if(constrain!=FALSE & length(getNames(x))!=1) stop("Multidimensional data is not supported for 'constrain!=FALSE'.")

    out                <- x
    reset_years        <- getYears(x, as.integer=TRUE) >= year
    out[,reset_years,] <- setYears(x[,rep(year,sum(reset_years)),], getYears(x[,reset_years,]))

    if(constrain=="first_use"){

      # determine first use
      first_use   <- toolConditionalReplace(magpply(x, found_first <- function(x){return(which(x!=0)[1])}, 1), "is.na()", 1)
      # find value of first use
      first_value <- as.magpie(as.array(x)[1:59199+(as.vector(first_use)-1)*59199], spatial=1)
      # set first use value for in all used years
      out[out==0 & x!=0] <- first_value[,rep(1, length(getYears(x))),][out==0 & x!=0]
    }

  return(out)
}
