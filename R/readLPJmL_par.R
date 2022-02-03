#' @title readLPJmL_par
#' @description Read parameter files from LPJmL model
#'
#' @param subtype Switch between different parameter files
#' @return List of magpie objects with results on global level
#'
#' @author Kristine Karstens
#' @examples
#' \dontrun{
#'   readSource("LPJmL_par", subtype="pft_lpjml4")
#' }
#'
#' @importFrom jsonlite fromJSON

readLPJmL_par <- function(subtype = "pft_lpjml4"){

  files <- c(pft_lpjml4        = "pft_lpjml4.js",
             pft_lpjml5        = "pft_lpjml5.js")

  file   <- toolSubtypeSelect(subtype, files)
  pftpar <- jsonlite::fromJSON(file)
  pftpar <- do.call(data.frame, subset(pftpar, pftpar$cultivation_type == 0,
                                select = c("name", "turnover")))
  names(pftpar) <- gsub("\\.", "_", names(pftpar))
  out    <- as.magpie(pftpar)

  return(out)
}
