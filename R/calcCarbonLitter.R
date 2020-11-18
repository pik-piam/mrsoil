#' @title calcCarbonLitter
#' @description Calculates Carbon Input from litter
#' @param litter_param litter scenario
#' @return List of magpie object with results on cellular level, weight on cellular level, unit and description.
#' @author Kristine Karstens
#' @examples
#'
#' \dontrun{
#'   calcOutput("CarbonLitter")
#' }
#' @importFrom magclass dimCode
#' @importFrom magpiesets findset
#' @importFrom stats quantile

calcCarbonLitter <- function(litter_param="default"){

  if(is.null(litter_param)) litter_param <- "default"

  litfallc <- readSource("LPJmL", subtype="LPJmL4:CRU_4.alitterfallc", convert="onlycorrect")[,findset("past_all"),]

  attributes   <- c("c","LC","NC")
  names        <- as.vector(outer("litfall", attributes, paste, sep="."))
  out          <- new.magpie(getCells(litfallc), getYears(litfallc), names, fill = 0)
  getSets(out) <- c("iso","cell","t","inputs","attributes")

  param.litter <- readSource("SOCbudgetParam", subtype=litter_param, convert=FALSE)

  out[,,"LC"] <- param.litter[,,"natveg_lgc_ratio"]
  out[,,"NC"] <- param.litter[,,"natveg_nc_ratio"]
  out[,,"c"]  <- litfallc

  out <- toolConditionalReplace(out, conditions = c("is.na()","<0", "is.infinite()"), replaceby = 0)

  ## Cut high input values at 95%-percentil
  #out[,,"NC"] <- toolConditionalReplace(out[,,"NC"], conditions = '> quantile(x, probs=0.95)', replaceby=eval(quantile(out[,,"NC"], probs=0.95)))


  return(list(x=out,
              weight=NULL,
              unit="tC per ha, tn per tc, tLn per tC",
              description=paste0("Carbon Input from Litter for natural vegetation"),
              min = 0,
              isocountries = FALSE))
}
