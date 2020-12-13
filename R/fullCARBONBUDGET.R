#' @title fullCARBONBUDGET
#' @description Function that produces the complete cellular data set of the SOC budget
#'
#' @param rev data revision which should be used as input (positive numeric).
#' @param dev flag to define carbon budget calculations
#'
#' @author Kristine Karstens
#'
#' @seealso
#' \code{\link{readSource}},\code{\link{getCalculations}},\code{\link{calcOutput}},\code{\link{setConfig}}
#'
#' @examples
#' \dontrun{
#'   retrieveData("CARBONBUDGET", rev=0, mainfolder="pathtowhereallfilesarestored")
#' }
#' @importFrom magclass setNames
#' @importFrom magpiesets findset
#' @import mrvalidation

fullCARBONBUDGET <- function(rev=0.1, dev=""){

  setConfig(regionmapping = NULL)
  setConfig(cachefolder = paste0("rev",rev), forcecache = TRUE)

  years <- sort(findset("past_all"))

  .cfg <- function(name) {
    ### Management Scenarios
    cfg <- list(manure       = "default",
                residue      = "default",
                rrecycle     = "default",
                yield        = "default",
                landuse      = "default",
                climate      = "default",
                tillage      = "histtill",
                litter_param = "CenturyAverage",
                soilinit     = "lu")

    if(grepl("constManure-",   name))  cfg$manure   <- gsub(".*(constManure-)(\\d{4}).*",  "freeze\\2",name)
    if(grepl("constResidues-", name))  cfg$residue  <- gsub(".*(constResidues-)(\\d{4}).*","freeze\\2",name)
    if(grepl("constYield-",    name))  cfg$yield    <- gsub(".*(constYield-)(\\d{4}).*",   "freeze\\2",name)
    if(grepl("constTillage-",  name))  cfg$tillage  <- gsub(".*(constTillage-)(.[^_]*).*","\\2",name)
    if(grepl("constResrate-",  name))  cfg$rrecycle <- gsub(".*(constResrate-)(\\d{4}).*", "freeze\\2",name)
    if(grepl("constLanduse-",  name))  cfg$landuse  <- gsub(".*(constLanduse-)(\\d{4}).*", "freeze\\2",name)
    if(grepl("constClimate-",  name))  cfg$climate  <- gsub(".*(constClimate-)(\\d{4}).*", "freeze\\2",name)

    if(grepl("constManagement2-",    name)){
      cfg$tillage  <- "mixedtill"
      cfg$rrecycle <- gsub(".*(constManagement2-)(\\d{4}).*", "freeze\\2",name)
      cfg$manure   <- gsub(".*(constManagement2-)(\\d{4}).*", "freeze\\2",name)
      cfg$yield    <- gsub(".*(constManagement2-)(\\d{4}).*", "freeze\\2",name)
    }
    if(grepl("constManagement-",   name)){
      cfg$tillage  <- "mixedtill"
      cfg$residue  <- gsub(".*(constManagement-)(\\d{4}).*", "freeze\\2",name)
      cfg$manure   <- gsub(".*(constManagement-)(\\d{4}).*", "freeze\\2",name)
    }
    if(grepl("Initial-", name))    cfg$soilinit     <- gsub(".*(Initial-)(.[^_]*).*","\\2",name)
    if(grepl("LitterPNV-", name))  cfg$litter_param <- gsub(".*(LitterPNV-)(.[^_]*).*","\\2",name)
    return(cfg)
  }
  cfg <- .cfg(dev)

  ### historic output only
  if(dev=="histManagement"){

    ### from mrcommons
    calcOutput("ResFieldBalancePast", cellular=TRUE, products="kres", aggregate=FALSE, scenario = cfg$yield, file="ResiduesAg_FieldBalance.rds")
    calcOutput("ResBiomass",          cellular=TRUE, aggregate=FALSE, scenario = cfg$yield, file="Residue_Biomass.rds")
    calcOutput("Production",          products="kcr",  cellular=TRUE, calibrated=TRUE, attributes="c", aggregate=FALSE, file="Crop_Harvest.rds")
    calcOutput("FAOmassbalance",      aggregate=FALSE, file="FAOmassbalance_ISO.rds")
    calcOutput("ManureRecyclingCroplandPast",     products="kli", cellular=TRUE, aggregate=FALSE, file="Manure_recycled.rds")
    calcOutput("Excretion",           cellular=TRUE,   attributes="npkc", aggregate=FALSE, file="Manure_excreted.rds")

    ### from mrsoil
    calcOutput("CarbonResidues", yieldscenario = cfg$yield, rec.scenario = cfg$rrecycle, res.scenario=cfg$residue, aggregate=FALSE, file="CarbonResidues.rds")
    calcOutput("CarbonManure",   scenario=cfg$manure, aggregate=FALSE, file="CarbonManure.rds")
    calcOutput("CarbonLitter",   litter_param=cfg$litter_param, climate_scen=cfg$climate, aggregate=FALSE, years=years, file="CarbonLitter.rds")
    calcOutput("CarbonInput",    cfg=cfg, aggregate=FALSE, years=years, file="CarbonInput.rds")
    calcOutput("Decay",          tillage=cfg$tillage, climate_scen=cfg$climate, aggregate=FALSE, years=years, file="Decay.rds")
    calcOutput("SteadyState",    cfg=cfg, aggregate=FALSE, output="full", years=years, file="SteadyState.rds")

    ### validation and post-processing
    calcOutput("ValidGridCarbonStocks", datasource = "WISE", aggregate="IPCC", file="WISE.rds")
    calcOutput("ValidGridCarbonStocks", datasource = "GSOC", aggregate="IPCC", file="GSOC.rds")
    calcOutput("ValidGridCarbonStocks", datasource = "SoilGrids", aggregate="IPCC", file="SoilGrids.rds")
    calcOutput("ValidGridCarbonStocks", datasource = "LPJmL4Paper", aggregate="IPCC", file="LPJmL4.rds")
    calcOutput("ClimateClass", source="IPCC_reduced" , aggregate=FALSE, file="IPCC_reduced.rds")
    calcOutput("ClimateClass", source="IPCC" ,         aggregate=FALSE, file="IPCC.rds")
    calcOutput("LPJmL", version="LPJmL4", climatetype="CRU_4", subtype="vegc", years=years, aggregate = FALSE, round=4, file="CarbonVegetation.rds")
  }


  ### historic & scenario output
  calcOutput("Landuse",       aggregate=FALSE, landuse_scen=cfg$landuse, years=years, file="Landuse.rds")
  calcOutput("LanduseChange", aggregate=FALSE, landuse_scen=cfg$landuse, years=years, file="LanduseChange.rds")
  calcOutput("SoilCarbon",    output="full", init=cfg$soilinit, cfg=cfg, aggregate=FALSE, years=years, file="SoilCarbon.rds")
}
