#' @title calcCarbonLitter
#' @description Calculates Carbon Input from litter
#' @param litter_param litter scenario
#' @param climate_scen climate configuration
#' @return List of magpie object with results on cellular level, weight on cellular level, unit and description.
#' @author Kristine Karstens
#' @examples
#' \dontrun{
#' calcOutput("CarbonLitter")
#' }
#' @importFrom magclass dimCode
#' @importFrom magpiesets findset
#' @importFrom stats quantile
#' @importFrom stringr str_split

calcCarbonLitter <- function(litter_param = "Century:Average:toC", climate_scen = "default") {

  # load and convert LPjmL data
  litfallc      <- readSource("LPJmL_new", subtype = "LPJmL4_for_MAgPIE_84a69edd:CRU4:historical:alitterfallc",
                              convert = "onlycorrect") * 0.01
  litburnc      <- readSource("LPJmL_new", subtype = "LPJmL4_for_MAgPIE_84a69edd:CRU4:historical:alitterburnc",
                              convert = "onlycorrect") * 0.01
  litfallc_wood <- readSource("LPJmL_new", subtype = "LPJmL4_for_MAgPIE_84a69edd:CRU4:historical:alitterfallc_wood",
                              convert = "onlycorrect") * 0.01
  litburnc_wood <- readSource("LPJmL_new", subtype = "LPJmL4_for_MAgPIE_84a69edd:CRU4:historical:alitterburnc_wood",
                              convert = "onlycorrect") * 0.01

  litfallc      <- toolConditionalReplace(litfallc - litburnc,           "<0", 0)
  litfallc_wood <- toolConditionalReplace(litfallc_wood - litburnc_wood, "<0", 0)
  rm(litburnc, litburnc_wood)

  litfallc      <- toolCoord2Isocell(mbind(setNames(litfallc - litfallc_wood, "leaf"),
                                           setNames(litfallc_wood,          "wood")))
  rm(litfallc_wood)

  ### add parameterization to litter carbon values from lpjml

  attributes   <- c("c", "LC", "NC")
  names        <- as.vector(outer(getNames(litfallc), attributes, paste, sep = "."))
  out          <- new.magpie(getCells(litfallc), getYears(litfallc), names, fill = 0)
  getSets(out) <- c("iso", "cell", "t", "inputs", "attributes")
  out[, , "wood.c"]  <- litfallc[, , "wood"]
  out[, , "leaf.c"]  <- litfallc[, , "leaf"]

  litter_param <- toolSplitSubtype(litter_param, list(source = c("Century", "Brovkin"),
                                                      type   = NULL,
                                                      config   = c("toDM", "toC", "ligRoot1",
                                                                   "ligRoot1p5", "ligRoot2")))

  if (litter_param$source == "Century") {

    # Use Average or specified plant functional type of CENTURY

    century      <- readSource("CENTURY", subtype = "tree", convert = FALSE)
    woody_parts  <- c("coarse_roots", "large_wood", "fine_branches")
    soft_tissue  <- c("leaves", "fine_roots")

    if (!(litter_param$type == "Average")) century <- collapseNames(century[, , str_split(litter_param$type, "_",
                                                                                          simplify = TRUE)[1]])
    unitTrans <- ifelse(litter_param$config == "toDM", 0.45, 1)

    out[, , "leaf.NC"] <- 1 / 68   # signif(mean(century[, , "nc_ratio"][, , soft_tissue]), 2)
    # CENTURY nitrogen too high, using LPJmL estimates
    out[, , "wood.NC"] <- 1 / 1025 # signif(mean(century[, , "nc_ratio"][, , woody_parts]), 2)
    # CENTURY nitrogen too high, using LPJmL estimates
    out[, , "leaf.LC"] <- signif(mean(century[, , "lgc_ratio"][, , soft_tissue]), 2) / unitTrans
    out[, , "wood.LC"] <- signif(mean(century[, , "lgc_ratio"][, , woody_parts]), 2) / unitTrans

  } else if (litter_param$source == "Brovkin") {

    # Use plant functional type specific (pft) values together with LPJmL data on pft distribution
    # to calculate mean litter parameterization per grid cell

    # Load foliage projected cover - fraction of pfts for each grid cell
    fpc        <- toolCoord2Isocell(readSource("LPJmL_new", subtype = "LPJmL4_for_MAgPIE_84a69edd:CRU4:historical:fpc",
                                               convert = "onlycorrect"))[, , "fraction natural vegetation",
                                                                         invert = TRUE]
    woody_pfts <- getNames(fpc[, , "grass", invert = TRUE, pmatch = TRUE])
    tree_frac  <- dimSums(fpc[, , woody_pfts], dim = 3)

    # Use turnover parameters per pft to calculate leaf fraction in soft tissue litter
    # (soft tissue = fine roots + leaves)
    lpjml_par       <- readSource("LPJmL_par", subtype = "pft_lpjml4",
                                  convert = FALSE)[, , "sapwood", invert = TRUE, pmatch = TRUE]
    leaf_fraction   <- collapseDim(lpjml_par[, , "turnover_root"] / dimSums(lpjml_par, dim = 3.2))

    # Load data from Brovkin et al. on leaf parameters (lignin and nitrogen concentration (per dry matter))
    brovkin   <- collapseDim(readSource("Brovkin", convert = FALSE),
                             dim = "LPJ_plant_functional_type")[, , "concentration", pmatch = TRUE]

    # Translate leaf parameters to fine root and wood parameters (including switch for lignin via litter_param)
    root2leaf_lig <-  c(ligRoot1 = 1,
                        ligRoot1p5 = 1.5,
                        ligRoot2 = 2)[litter_param$config]

    root2leaf_n   <- 1 / 1.16 #from lpjml (par.pft - "ratio" for "root")
    wood2leaf_n   <- 1 / 13.5 #from lpjml (par.pft - "ratio" for "sapwood")

    # Set static values for lignin fraction for broad- and needleleaved pfts
    # following M.M. Rahman et al, 2012 (https://doi.org/10.1080/02757540.2013.790380)
    lig_wood_par <- new.magpie(names = woody_pfts)
    lig_wood_par[, , "broadleaved",  pmatch = TRUE] <- 0.22
    lig_wood_par[, , "needleleaved", pmatch = TRUE] <- 0.29

    # C concentration (per dry matter) to transform lignin and nitrogen concentrations to LC and NC values
    c_concentration <- 0.45 #from lpjml

    # Put everything together
    out[, , "leaf.NC"] <- dimSums(fpc * (    leaf_fraction  * brovkin[, , "n_concentration"] / 100 +
                                        (1 - leaf_fraction) * brovkin[, , "n_concentration"] / 100 * root2leaf_n),
                                  dim = 3.1) / c_concentration

    out[, , "leaf.LC"] <- dimSums(fpc * (    leaf_fraction  * brovkin[, , "lig_concentration"] / 100 +
                                        (1 - leaf_fraction) * brovkin[, , "lig_concentration"] / 100 * root2leaf_lig),
                                  dim = 3.1) / c_concentration

    out[, , "wood.NC"] <- dimSums(fpc[, , woody_pfts] / tree_frac *
                                    brovkin[, , woody_pfts][, , "n_concentration"] / 100 * wood2leaf_n,
                                  dim = 3.1) / c_concentration

    out[, , "wood.LC"] <- dimSums(fpc[, , woody_pfts] / tree_frac * lig_wood_par, dim = 3.1) / c_concentration


  } else stop(paste("Litter parameterization source unknown:", litter_param$source))

  out <- toolConditionalReplace(out, conditions = c("is.na()", "<0", "is.infinite()"), replaceby = 0)

  if (grepl("freeze", climate_scen)) {
    freeze_year <- as.integer(gsub("freeze", "", climate_scen))
    out         <- toolFreezeAverage(out, freeze_year)
  }

  out <- out[, sort(findset("past_soc")), ]

  return(list(x = out,
              weight = NULL,
              unit = "tC per ha, tn per tc, tLn per tC",
              description = paste0("Carbon Input from Litter for natural vegetation"),
              min = 0,
              isocountries = FALSE))
}
