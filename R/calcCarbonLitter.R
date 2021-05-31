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

  # load and convert LPjmL data
  litfallc      <- readSource("LPJmL_new", subtype="LPJmL4_for_MAgPIE_84a69edd:CRU4:historical:alitterfallc",        convert="onlycorrect")*0.01
  litburnc      <- readSource("LPJmL_new", subtype="LPJmL4_for_MAgPIE_84a69edd:CRU4:historical:alitterburnc",        convert="onlycorrect")*0.01
  litfallc_wood <- readSource("LPJmL_new", subtype="LPJmL4_for_MAgPIE_84a69edd:CRU4:historical:alitterfallc_wood",   convert="onlycorrect")*0.01
  litburnc_wood <- readSource("LPJmL_new", subtype="LPJmL4_for_MAgPIE_84a69edd:CRU4:historical:alitterburnc_wood",   convert="onlycorrect")*0.01

  litfallc      <- toolConditionalReplace(litfallc-litburnc,           "<0", 0)
  litfallc_wood <- toolConditionalReplace(litfallc_wood-litburnc_wood, "<0", 0)
  rm(litburnc, litburnc_wood)

  litfallc      <- toolCoord2Isocell(mbind(setNames(litfallc-litfallc_wood, "leaf"),
                                           setNames(litfallc_wood,          "wood")))
  rm(litfallc_wood)

  attributes   <- c("c","LC","NC")
  names        <- as.vector(outer(getNames(litfallc), attributes, paste, sep="."))
  out          <- new.magpie(getCells(litfallc), getYears(litfallc), names, fill = 0)
  getSets(out) <- c("iso","cell","t","inputs","attributes")

  tmp          <- readSource("CENTURY", subtype="tree", convert=FALSE)
  names        <- as.vector(outer(c("leaf","wood"), getItems(tmp, dim=3.3), FUN = "paste", sep="."))
  param.litter <- new.magpie("GLO",NULL, names, fill = 0)
  woody_parts  <- c("coarse_roots","large_wood","fine_branches")
  soft_tissue  <- c("leaves","fine_roots")

  if(!litter_param=="CenturyAverage") tmp <- collapseNames(tmp[,,litter_param])

  param.litter[,,"leaf.nc_ratio"]  <- signif(mean(tmp[,,"nc_ratio"][,,soft_tissue]),2)
  param.litter[,,"leaf.lgc_ratio"] <- signif(mean(tmp[,,"lgc_ratio"][,,soft_tissue]),2)
  param.litter[,,"wood.nc_ratio"]  <- signif(mean(tmp[,,"nc_ratio"][,,woody_parts]),2)
  param.litter[,,"wood.lgc_ratio"] <- signif(mean(tmp[,,"lgc_ratio"][,,woody_parts]),2)

  out[,,"wood.LC"] <- param.litter[,,"wood.lgc_ratio"]
  out[,,"wood.NC"] <- 1/1025    #param.litter[,,"wood.nc_ratio"] - CENTURY nitrogen too high, using LPJmL estimates
  out[,,"wood.c"]  <- litfallc[,,"wood"]

  out[,,"leaf.LC"] <- param.litter[,,"leaf.lgc_ratio"]
  out[,,"leaf.NC"] <- 1/68      #param.litter[,,"leaf.nc_ratio"] - CENTURY nitrogen too high, using LPJmL estimates
  out[,,"leaf.c"]  <- litfallc[,,"leaf"]

  out <- toolConditionalReplace(out, conditions = c("is.na()","<0", "is.infinite()"), replaceby = 0)

  if(grepl("freeze", climate_scen)){
    freeze_year <- as.integer(gsub("freeze","",climate_scen))
    out         <- toolFreezeAverage(out, freeze_year)
  }

  out <- out[,sort(findset("past_soc")),]

  return(list(x=out,
              weight=NULL,
              unit="tC per ha, tn per tc, tLn per tC",
              description=paste0("Carbon Input from Litter for natural vegetation"),
              min = 0,
              isocountries = FALSE))
}
