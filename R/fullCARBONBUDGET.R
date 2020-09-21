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

  options(manure  ="default")
  options(residue ="default")
  options(yield   ="default")
  options(tillage ="histtill")
  options(litter_param="default")


  if(grepl("manure_*", dev)){           options(manure= gsub("manure_","",dev))
  } else if(grepl("residue_*", dev)){   options(residue= gsub("residue_","",dev))
  } else if(grepl("yield_*",   dev)){   options(yield= gsub("yield_","",dev))
  } else if(grepl("tillage_*", dev)){   options(tillage= gsub("tillage_","",dev))
  }

  ### from mrcommons
  calcOutput("ResFieldBalancePast", cellular=TRUE, products="kres", aggregate=FALSE, file="ResiduesAg_FieldBalance.rds")
  calcOutput("ResBiomass", cellular=TRUE, aggregate=FALSE, file="Residue_Biomass.rds")
  calcOutput("Production", products="kcr", cellular=TRUE, calibrated=TRUE, attributes="c", aggregate=FALSE, file="Crop_Harvest.rds")
  calcOutput("FAOmassbalance", aggregate=FALSE, file="FAOmassbalance_ISO.rds")
  calcOutput("ManureRecyclingCroplandPast", products="kli", cellular=TRUE, aggregate=FALSE, file="Manure_recycled.rds")
  calcOutput("Excretion", cellular=TRUE, attributes="npkc", aggregate=FALSE, file="Manure_excreted.rds")

  ### from mrSOCbudget
  calcOutput("CarbonResidues", aggregate=FALSE, file="CarbonResidues.rds")
  calcOutput("CarbonManure",   aggregate=FALSE, file="CarbonManure.rds")
  calcOutput("CarbonLitter",   aggregate=FALSE, file="CarbonLitter.rds")
  calcOutput("CarbonInput",    aggregate=FALSE, landtype="all", file="CarbonInput.rds")

  calcOutput("Landuse",       aggregate=FALSE, file="Landuse.rds")
  calcOutput("LanduseChange", aggregate=FALSE, file="LanduseChange.rds")
  calcOutput("SoilCarbon",    output="full", aggregate=FALSE, file="SoilCarbon.rds")
}
