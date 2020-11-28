#' @title fullCARBONBUDGET
#' @description Function that produces the complete cellular data set of the SOC budget
#'
#' @param rev data revision which should be used as input (positive numeric).
#' @param dev flag to mark carbon budget calculations
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

fullCARBONBUDGET <- function(rev=0.1, dev="default"){

  setConfig(regionmapping = NULL)

  .cfg <- function(dev) {
    ### Management Scenarios
    cfg <- list(manure       = "default",
                residue      = "default",
                rrecycle     = "default",
                yield        = "default",
                landuse      = "default",
                climate      = "default",
                tillage      = "histtill",
                litter_param = "default",
                soilinit     = "lu")

    if(grepl("manure_",   dev)) cfg$manure   <- gsub(".*(manure_)(.[^_]*\\d{4}).*","\\2",dev)
    if(grepl("residue_",  dev)) cfg$residue  <- gsub(".*(residue_)(.[^_]*\\d{4}).*","\\2",dev)
    if(grepl("yield_",    dev)) cfg$yield    <- gsub(".*(yield_)(.[^_]*\\d{4}).*","\\2",dev)
    if(grepl("tillage_",  dev)) cfg$tillage  <- gsub(".*(tillage_)(.[^_]*).*","\\2",dev)
    if(grepl("rrecycle_", dev)) cfg$rrecycle <- gsub(".*(rrecycle_)(.[^_]*\\d{4}).*","\\2",dev)
    if(grepl("landuse_",  dev)) cfg$landuse  <- gsub(".*(landuse_)(.[^_]*\\d{4}).*","\\2",dev)
    if(grepl("climate_",  dev)) cfg$climate  <- gsub(".*(climate_)(.[^_]*\\d{4}).*","\\2",dev)

    if(grepl("alloff_",    dev)){
      cfg$tillage  <- "mixedtill"
      cfg$rrecycle <- gsub(".*(alloff_)(.[^_]*\\d{4}).*","\\2",dev)
      cfg$manure   <- gsub(".*(alloff_)(.[^_]*\\d{4}).*","\\2",dev)
      cfg$yield    <- gsub(".*(alloff_)(.[^_]*\\d{4}).*","\\2",dev)
    }
    if(grepl("alloff2_",   dev)){
      cfg$tillage  <- "mixedtill"
      cfg$residue  <- gsub(".*(alloff2_)(.[^_]*\\d{4}).*","\\2",dev)
      cfg$manure   <- gsub(".*(alloff2_)(.[^_]*\\d{4}).*","\\2",dev)
    }
    if(grepl("init_", dev))       cfg$soilinit     <- gsub(".*(init_)(.[^_]*).*","\\2",dev)
    if(grepl("litterPNV_", dev))  cfg$litter_param <- gsub(".*(litterPNV_)(.[^_]*_.[^_]*).*","\\2",dev)
    return(cfg)
  }
  cfg <- .cfg(dev)


  ### from mrcommons
  calcOutput("ResFieldBalancePast", cellular=TRUE, products="kres", aggregate=FALSE, file="ResiduesAg_FieldBalance.rds")
  calcOutput("ResBiomass",          cellular=TRUE, aggregate=FALSE, file="Residue_Biomass.rds")
  calcOutput("Production",     products="kcr",  cellular=TRUE, calibrated=TRUE, attributes="c", aggregate=FALSE, file="Crop_Harvest.rds")
  calcOutput("FAOmassbalance", aggregate=FALSE, file="FAOmassbalance_ISO.rds")
  calcOutput("ManureRecyclingCroplandPast",     products="kli", cellular=TRUE, aggregate=FALSE, file="Manure_recycled.rds")
  calcOutput("Excretion",      cellular=TRUE,   attributes="npkc", aggregate=FALSE, file="Manure_excreted.rds")

  ### from mrSOCbudget
  calcOutput("CarbonResidues", yieldscenario = cfg$yield, rec.scenario = cfg$rrecycle, res.scenario=cfg$residue, aggregate=FALSE, file="CarbonResidues.rds")
  calcOutput("CarbonManure",   scenario=cfg$manure, aggregate=FALSE, file="CarbonManure.rds")
  calcOutput("CarbonLitter",   litter_param=cfg$litter_param, climate_scen=cfg$climate, aggregate=FALSE, file="CarbonLitter.rds")
  calcOutput("CarbonInput",    cfg=cfg, aggregate=FALSE, file="CarbonInput.rds")

  calcOutput("Landuse",       aggregate=FALSE, landuse_scen=cfg$landuse, file="Landuse.rds")
  calcOutput("LanduseChange", aggregate=FALSE, landuse_scen=cfg$landuse, file="LanduseChange.rds")
  calcOutput("SoilCarbon",    output="full", init=cfg$soilinit, cfg=cfg, aggregate=FALSE, file="SoilCarbon.rds")
}
