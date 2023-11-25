#' @title calcParamResidues
#' @description Bring all parameter settings (lignin, nitrogen) for residues together
#'
#' @param source      "IPCC"          for IPCC Guideline values
#'                    "IPCC+woody"    for IPCC Guideline values + Feedipedia for woody
#' @return List of magpie object with results on global level, unit and description.
#' @author Kristine Karstens
#'
#' @examples
#' \dontrun{
#' calcOutput("ParamResidues")
#' }

calcParamResidues <- function(source = "IPCC+woody") {

  kcr  <- magpiesets::findset("kcr")
  c2dm <- 0.45
  if(grepl("IPCC", source)){

    # Load IPCC parameters and create mapping
    param     <- readSource("IPCC", subtype = "residues_table5p5b", convert = FALSE)
    generic   <- "Generic value for crops not indicated below"
    kcr2T5p5B <- list(begr        = generic,
                      oilpalm     = generic,
                      trce        = "Generic Grains",
                      rice_pro    = "Rice",
                      betr        = generic,
                      tece        = "Generic Grains",
                      soybean     = "Soybeans",
                      sunflower   = generic,
                      sugr_cane   = generic,
                      rapeseed    = generic,
                      others      = generic,
                      maiz        = "Maize",
                      cottn_pro   = generic,
                      cassav_sp   = "Potatoes and Tubers",
                      puls_pro    = "Beans and Pulses",
                      potato      = "Potatoes and Tubers",
                      sugr_beet   = "Potatoes and Tubers",
                      foddr       = c("Non-N-fixing forages", "N-fixing forages"),
                      groundnut   = "Peanuts")

    # Create and fill output object
    names        <- as.vector(outer(kcr, c("NC", "LC"), paste, sep = "."))
    out          <- new.magpie("GLO", NULL, names, fill = 0,
                               sets = c("region", "year", "kcr", "attributes"))
    for(k in kcr) {
      # Take mean value over all categories and divide by carbon density
      out[ , , k] <- as.vector(dimSums(param[ , , kcr2T5p5B[[k]]], dim = 3.1)  / c2dm / length(kcr2T5p5B[[k]]))
    }

    if(source == "IPCC+woody"){
      # Assume higher lignin content of residue for crop trees
      woodyTypes <- c("betr", "oilpalm")
      LCwoody    <- 0.145 / c2dm # 0.145 lignin content from feedipedia.org (mean value of oil palm residues)
      out[,, "LC"][,, woodyTypes] <- LCwoody
    }

  } else {
    stop("'source' unknown.")
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = "tN per tC, tLn per tC",
              description  = "Lignin and Nitrogen ratios for Residues",
              min          = 0))
}
