#' @title fullCARBONBUDGET
#' @description Function that produces the complete cellular data set of the SOC budget
#'
#' @param rev data revision which should be used as input (positive numeric).
#' @param dev flag to define carbon budget calculations
#'
#' @author Kristine Karstens
#'
#' @seealso
#' \code{\link{readSource}},\code{\link{getCalculations}},\code{\link{calcOutput}}
#'
#' @examples
#' \dontrun{
#' retrieveData("CARBONBUDGET", rev = 0, mainfolder = "pathtowhereallfilesarestored")
#' }
#' @importFrom magclass setNames
#' @importFrom magpiesets findset
#' @import mrvalidation
#' @import mrmagpie

fullCARBONBUDGET <- function(rev = 0.1, dev = "") {

  years <- sort(findset("past_all"))

  .cfg <- function(name, init = FALSE) {
    ### Management Scenarios
    cfg <- list(manure       = "default",
                residue      = "default",
                rrecycle     = "default",
                yield        = "default",
                landuse      = "default",
                climate      = "default",
                tillage      = "histtill",
                litter_param = "Century:Average:toC",
                soilinit     =  1510)

    if (grepl("LitterPNV-", name)) cfg$litter_param <- gsub(".*(LitterPNV-)(.[^_]*).*", "\\2", name)

    if (!init){ # ignore settings for spinup
      if (grepl("constManure-",   name)) cfg$manure   <- gsub(".*(constManure-)(\\d{4}).*",  "freeze\\2", name)
      if (grepl("constResidues-", name)) cfg$residue  <- gsub(".*(constResidues-)(\\d{4}).*", "freeze\\2", name)
      if (grepl("constYield-",    name)) cfg$yield    <- gsub(".*(constYield-)(\\d{4}).*",   "freeze\\2", name)
      if (grepl("constTillage-",  name)) cfg$tillage  <- gsub(".*(constTillage-)(.[^_]*).*", "\\2", name)
      if (grepl("constResrate-",  name)) cfg$rrecycle <- gsub(".*(constResrate-)(\\d{4}).*", "freeze\\2", name)
      if (grepl("constLanduse-",  name)) cfg$landuse  <- gsub(".*(constLanduse-)(\\d{4}).*", "freeze\\2", name)
      if (grepl("constClimate-",  name)) cfg$climate  <- gsub(".*(constClimate-)(\\d{4}).*", "freeze\\2", name)

      if (grepl("constManagement2-",    name)) {
        cfg$tillage  <- "mixedtill"
        cfg$rrecycle <- gsub(".*(constManagement2-)(\\d{4}).*", "freeze\\2", name)
        cfg$manure   <- gsub(".*(constManagement2-)(\\d{4}).*", "freeze\\2", name)
        cfg$yield    <- gsub(".*(constManagement2-)(\\d{4}).*", "freeze\\2", name)
      }
      if (grepl("constManagement-",   name)) {
        cfg$tillage  <- "mixedtill"
        cfg$residue  <- gsub(".*(constManagement-)(\\d{4}).*", "freeze\\2", name)
        cfg$manure   <- gsub(".*(constManagement-)(\\d{4}).*", "freeze\\2", name)
      }
      if (grepl("Initial-", name)) cfg$soilinit     <- as.numeric(gsub(".*(Initial-)(.[^_]*).*", "\\2", name))
    }

    return(cfg)
  }

  cfg        <- .cfg(dev)
  cfgInit <- .cfg(dev, init = TRUE)

  ### historic output only
  if (grepl("histManagement", dev)) {

    ### from mrcommons
    calcOutput("ResFieldBalancePast", cellular = TRUE, products = "kres", aggregate = FALSE,
               scenario = cfg$yield, file = "ResiduesAg_FieldBalance.rds")
    calcOutput("ResBiomass",          cellular = TRUE, aggregate = FALSE,
               scenario = cfg$yield, file = "Residue_Biomass.rds")
    calcOutput("Production",     products = "kcr",  cellular = TRUE, calibrated = TRUE,
               attributes = "c", aggregate = FALSE, file = "Crop_Harvest.rds")
    calcOutput("FAOmassbalance",      aggregate = FALSE, file = "FAOmassbalance_ISO.rds")
    calcOutput("ManureRecyclingCroplandPast",     products = "kli", cellular = TRUE,
               aggregate = FALSE, file = "Manure_recycled.rds")
    calcOutput("Excretion",           cellular = TRUE,   attributes = "npkc",
               aggregate = FALSE, file = "Manure_excreted.rds")

    ### from mrsoil
    calcOutput("CarbonResidues",   yieldscenario = cfg$yield, rec.scenario = cfg$rrecycle,
               res.scenario = cfg$residue, aggregate = FALSE, file = "CarbonResidues.rds")
    calcOutput("CarbonManure",     scenario = cfg$manure, aggregate = FALSE, file = "CarbonManure.rds")
    calcOutput("CarbonLitter",     litter_param = cfg$litter_param, climate_scen = cfg$climate,
               aggregate = FALSE, years = years, file = "CarbonLitter.rds")
    calcOutput("CarbonInput",      cfg = cfg, aggregate = FALSE, years = years, file = "CarbonInput.rds")
    calcOutput("Decay",            tillage = cfg$tillage, climate_scen = cfg$climate, aggregate = FALSE,
               years = years, file = "Decay.rds")
    calcOutput("SteadyState",      cfg = cfg, aggregate = FALSE, output = "reduced", years = years,
               file = "SteadyState.rds")
    calcOutput("SteadyState",      cfg = cfg, aggregate = FALSE, output = "alpha_in", years = years,
               file = "CarbonInflow.rds")
    calcOutput("SoilCarbonSpinup", cfg_default = cfgInit, file = "SoilCarbonSpinup.rds")

    ### validation and post-processing
    calcOutput("ValidGridSOCStocks", datasource = "WISE", aggregate = "IPCC", file = "WISE.rds")
    calcOutput("ValidGridSOCStocks", datasource = "GSOC", aggregate = "IPCC", file = "GSOC.rds")
    calcOutput("ValidGridSOCStocks", datasource = "SoilGrids", aggregate = "IPCC", file = "SoilGrids.rds")
    calcOutput("ValidGridSOCStocks", datasource = "LPJmL4Paper", aggregate = "IPCC", file = "LPJmL4.rds")
    calcOutput("ValidGridSOCStocks", datasource = "SOCDebtPaper", aggregate = "IPCC", file = "SOCDebtPaperC.rds")
    calcOutput("ValidGridSOCStocks", datasource = "SoilGrids2:new",
               intensive = TRUE, aggregate = FALSE, file = "SoilGrids2.rds")
    calcOutput("ValidGridSOCStocks", datasource = "SoilGrids2:q05_new",
               intensive = TRUE, aggregate = FALSE, file = "SoilGrids2_Q0p05.rds")
    calcOutput("ValidGridSOCStocks", datasource = "SoilGrids2:q95_new",
               intensive = TRUE, aggregate = FALSE, file = "SoilGrids2_Q0p95.rds")
    calcOutput("ValidGridSOCStocks", datasource = "LPJmL4_for_MAgPIE_84a69edd:CRU4:historical",
               intensive = TRUE, aggregate = FALSE, file = "LPJmL4_CRU.rds")
    calcOutput("SOCDebt", aggregate = FALSE, file = "SOCDebtSanderman.rds")

    calcOutput("ClimateClass", source = "IPCC_reduced", aggregate = FALSE, file = "IPCC_reduced.rds")
    calcOutput("ClimateClass", source = "IPCC",         aggregate = FALSE, file = "IPCC.rds")
    calcOutput("LPJmL", version = "LPJmL4", climatetype = "CRU_4", subtype = "vegc",
               years = years, aggregate = FALSE, round = 4, file = "CarbonVegetation.rds")
  }

  ### historic & scenario output
  calcOutput("Landuse", aggregate = FALSE, landuse_scen = cfg$landuse,
             years = years, file = "Landuse.rds")
  calcOutput("Landuse", aggregate = FALSE, landuse_scen = cfg$landuse, output = "change",
             years = years, file = "LanduseChange.rds")
  calcOutput("SoilCarbon", output = "full", cfg = cfg, cfg_default = cfgInit,
             aggregate = FALSE, years = years, file = "SoilCarbon.rds")
}
