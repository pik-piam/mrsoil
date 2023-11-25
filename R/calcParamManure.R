#' @title calcParamManure
#' @description Bring all parameter settings (lignin, nitrogen) for residues together
#'
#' @param source      "IPCC"          for IPCC Guideline values
#' @return List of magpie object with results on global level, unit and description.
#' @author Kristine Karstens
#'
#' @examples
#' \dontrun{
#' calcOutput("ParamManure")
#' }

calcParamManure <- function(source = "IPCC") {

  kli  <- magpiesets::findset("kli")
  c2dm <- 0.45
  if(grepl("IPCC", source)){

    param              <- readSource("IPCC", subtype = "manure_table5p5c", convert = FALSE)

    # Create and fill output object
    names        <- as.vector(outer(kli, c("NC", "LC"), paste, sep = "."))
    out          <- new.magpie("GLO", NULL, names, fill = 0,
                               sets = c("region", "year", "kcr", "attributes"))
    out[, , "NC"]  <- 1 / param[, , "cn_ratio"]
    out[, , "LC"]  <- param[, , "LC_dm"] / c2dm / 100

  } else {
    stop("'source' unknown.")
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = "tN per tC, tLn per tC",
              description  = "Lignin and Nitrogen ratios for Manure",
              min          = 0))
}
