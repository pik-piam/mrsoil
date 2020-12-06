#' @title calcCarbonLitter
#' @description Calculates Carbon Input from litter
#' @param litter_param litter scenario
#' @param climate_scen climate configuration
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

calcCarbonLitter <- function(litter_param="CenturyAverage", climate_scen="default"){

  if(is.null(litter_param)) litter_param <- "CenturyAverage"

  litfallc <- readSource("LPJmL", subtype="LPJmL4:CRU_4.alitterfallc", convert="onlycorrect")[,sort(findset("past_soc")),]

  attributes   <- c("c","LC","NC")
  names        <- as.vector(outer("litfall", attributes, paste, sep="."))
  out          <- new.magpie(getCells(litfallc), getYears(litfallc), names, fill = 0)
  getSets(out) <- c("iso","cell","t","inputs","attributes")

  if(litter_param=="PerennialGrasses"){

    # from TABLE 5.5B (NEW GUIDANCE) of the 2019 refinement off the ipcc guidelines vol. 4 from 2006
    param.litter <- new.magpie("GLO",NULL,c("nc_ratio", "lgc_ratio"), fill=signif(c(0.0126/0.44, 0.049/0.44),2))

  } else if(litter_param=="CenturyAverage"){

    tmp          <- readSource("CENTURY", subtype="tree", convert=FALSE)
    param.litter <- collapseNames(tmp[,,1:2])
    param.litter[,,"nc_ratio"]  <- signif(mean(tmp[,,"nc_ratio"]),2)
    param.litter[,,"lgc_ratio"] <- signif(mean(tmp[,,"lgc_ratio"]),2)

  } else {

    param.litter <- signif(collapseNames(readSource("CENTURY", subtype="tree", convert=FALSE)[,,litter_param]),2)
  }

  out[,,"LC"] <- param.litter[,,"lgc_ratio"]
  out[,,"NC"] <- param.litter[,,"nc_ratio"]
  out[,,"c"]  <- litfallc

  out <- toolConditionalReplace(out, conditions = c("is.na()","<0", "is.infinite()"), replaceby = 0)

  ## Cut high input values at 95%-percentil
  #out[,,"NC"] <- toolConditionalReplace(out[,,"NC"], conditions = '> quantile(x, probs=0.95)', replaceby=eval(quantile(out[,,"NC"], probs=0.95)))

  if(grepl("freeze", climate_scen)){
    freeze_year <- as.integer(gsub("freeze","",climate_scen))
    reset_years <- getYears(out, as.integer=TRUE) >= freeze_year
    out[,reset_years,] <- setYears(out[,rep(freeze_year,sum(reset_years)),], getYears(out[,reset_years,]))
  }

  return(list(x=out,
              weight=NULL,
              unit="tC per ha, tn per tc, tLn per tC",
              description=paste0("Carbon Input from Litter for natural vegetation"),
              min = 0,
              isocountries = FALSE))
}
