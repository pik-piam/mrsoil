#' @title calcCarbonLitter
#' @description Calculates Carbon Input from litter
#'
#' @param lpjmlNatveg Switch between LPJmL natveg versionstop
#' @param climatetype Switch between different climate scenarios
#' @param mode        "historicalSpinup" for historical period and
#'                    "magpieInput" for future
#' @param fixFpc      if TRUE using fixed (old) fpc data
#'
#' @return List of magpie object with results on cellular level, weight on cellular level, unit and description.
#' @author Kristine Karstens
#' @examples
#' \dontrun{
#' calcOutput("CarbonLitter", aggregate = FALSE)
#' }
#' @importFrom magpiesets findset

calcCarbonLitter <-  function(lpjmlNatveg = "LPJmL4_for_MAgPIE_44ac93de",
                              climatetype = "GSWP3-W5E5:historical",
                              mode        = "historicalSpinup",
                              fixFpc      = FALSE) {

  mode2stage  <- c(historicalSpinup     = "raw1901",
                   magpieInput          = "harmonized2020")

  stage       <- mode2stage[mode]

  # load and convert LPjmL data
  litfallc     <- calcOutput("LPJmL_new", version = lpjmlNatveg, climatetype = climatetype, stage = stage,
                              subtype = "alitterfallc", aggregate = FALSE, )
  litburnc     <- calcOutput("LPJmL_new", version = lpjmlNatveg, climatetype = climatetype, stage = stage,
                              subtype = "alitterburnc", aggregate = FALSE)
  litfallcWood <- calcOutput("LPJmL_new", version = lpjmlNatveg, climatetype = climatetype, stage = stage,
                              subtype = "alitterfallc_wood", aggregate = FALSE)
  litburncWood <- calcOutput("LPJmL_new", version = lpjmlNatveg, climatetype = climatetype, stage = stage,
                              subtype = "alitterburnc_wood", aggregate = FALSE)

  litfallc     <- toolConditionalReplace(litfallc - litburnc,         "<0", 0)
  litfallcWood <- toolConditionalReplace(litfallcWood - litburncWood, "<0", 0)
  rm(litburnc, litburncWood)

  litfallc     <- mbind(setNames(litfallc - litfallcWood, "leaf"),
                        setNames(litfallcWood,            "wood"))
  rm(litfallcWood)

  ### add parameterization to litter carbon values from lpjml

  attributes   <- c("c", "LC", "NC")
  names        <- as.vector(outer(getNames(litfallc), attributes, paste, sep = "."))
  out          <- new.magpie(getCells(litfallc), getYears(litfallc), names, fill = 0)
  getSets(out, fulldim = FALSE)[3] <- "inputs.attributes"
  out[, , "wood.c"]  <- litfallc[, , "wood"]
  out[, , "leaf.c"]  <- litfallc[, , "leaf"]

  # Use plant functional type specific (pft) values together with LPJmL data on pft distribution
  # to calculate mean litter parameterization per grid cell

  # Load foliage projected cover - fraction of pfts for each grid cell
  fpc <- calcOutput("LPJmL_new",
                    version     = ifelse(fixFpc, "LPJmL4_for_MAgPIE_84a69edd", lpjmlNatveg),
                    climatetype = ifelse(fixFpc, "GSWP3-W5E5:historical", climatetype),
                    stage       = ifelse(fixFpc & (mode != "historicalSpinup"), "smoothed", stage),
                    subtype = "fpc", aggregate = FALSE)[, , "fraction natural vegetation", invert = TRUE]
  if(fixFpc) fpc <- toolFillYears(fpc, years = getYears(out))

  woodyPfts <- getNames(fpc[, , "grass", invert = TRUE, pmatch = TRUE])
  treeFrac  <- dimSums(fpc[, , woodyPfts], dim = 3)

  # Use turnover parameters per pft to calculate leaf fraction in soft tissue litter
  # (soft tissue = fine roots + leaves)
  lpjmlPar       <- readSource("LPJmL_par", subtype = "pft_lpjml4",
                                convert = FALSE)[, , "sapwood", invert = TRUE, pmatch = TRUE]
  leafFrac   <- collapseDim(lpjmlPar[, , "turnover_root"] / dimSums(lpjmlPar, dim = 3.2))

  # Load data from Brovkin et al. on leaf parameters (lignin and nitrogen concentration (per dry matter))
  brovkin   <- collapseDim(readSource("Brovkin", convert = FALSE),
                           dim = "LPJ_plant_functional_type")[, , "concentration", pmatch = TRUE]

  # Translate leaf parameters to fine root and wood parameters
  # Following Guo. et al., 2021 (extracted from Fig. 5(e))
  root2leafLig <-  function(leafLig) return(13 + 0.9 * leafLig) # leafLig as fraction

  # Following LPJmL parameter specification
  root2leafN   <- 1 / 1.16 # from lpjml (par.pft - "ratio" for "root")
  wood2leafN   <- 1 / 13.5 # from lpjml (par.pft - "ratio" for "sapwood")

  # Set static values for lignin fraction for broad- and needleleaved pfts
  # following M.M. Rahman et al, 2012 (https://doi.org/10.1080/02757540.2013.790380)
  ligWoodPar <- new.magpie(names = woodyPfts)
  ligWoodPar[, , "broadleaved",  pmatch = TRUE] <- 0.22
  ligWoodPar[, , "needleleaved", pmatch = TRUE] <- 0.29

  # C concentration (per dry matter) to transform lignin and nitrogen concentrations to LC and NC values
  cConcentration <- 0.45 # from lpjml

  # Put everything together
  out[, , "leaf.NC"] <- dimSums(fpc * (leafFrac  * brovkin[, , "n_concentration"] / 100 +
                                         (1 - leafFrac) * brovkin[, , "n_concentration"] / 100 * root2leafN),
                                dim = 3.1) / cConcentration

  out[, , "leaf.LC"] <- dimSums(fpc * (leafFrac  * brovkin[, , "lig_concentration"] / 100 +
                                         (1 - leafFrac) * root2leafLig(brovkin[, , "lig_concentration"]) / 100),
                                dim = 3.1) / cConcentration

  out[, , "wood.NC"] <- dimSums(fpc[, , woodyPfts] / treeFrac *
                                  brovkin[, , woodyPfts][, , "n_concentration"] / 100 * wood2leafN,
                                dim = 3.1) / cConcentration

  out[, , "wood.LC"] <- dimSums(fpc[, , woodyPfts] / treeFrac * ligWoodPar, dim = 3.1) / cConcentration

  out <- toolConditionalReplace(out, conditions = c("is.na()", "<0", "is.infinite()"), replaceby = 0)

  #weights -----> unclear!


  return(list(x            = out,
              weight       = NULL,
              unit         = "tC per ha, tn per tc, tLn per tC",
              description  = paste0("Carbon Input from Litter for natural vegetation"),
              min          = 0,
              isocountries = FALSE))
}
