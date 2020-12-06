#' @title downloadCENTURY
#' @description This function download the CENTURY model (available at 'https://www2.nrel.colostate.edu/projects/century/century.tar.Z')
#'
#' @author Kristine Karstens
#'
#' @seealso
#' \code{\link{downloadCENTURY}}
#'
#' @examples
#' \dontrun{ downloadSource("CENTURY") }

downloadCENTURY <- function() {

  settings <- list(title = "CENTURY model obtained from https://www2.nrel.colostate.edu/projects/century/obtain2.htm",
                     url = "https://www2.nrel.colostate.edu/projects/century/century.tar.Z",
                     doi = "")

  download.file(settings$url, destfile="century.tar.Z", mode="wb") # use cent40_src.zip for windows
  untar("century.tar.Z", files = "PARAMETER_FILES/")
  unlink("century.tar.Z")

  return(list(url           = settings$url,
              doi           = settings$doi,
              title         = settings$title,
              author        = "Colorado State University",
              version       = NULL,
              release_date  = "2000-06-11",
              license       = NULL,
              reference     = NULL))
}
