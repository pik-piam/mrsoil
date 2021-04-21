#' @title readCENTURY
#' @description Read parameter files in CENTURY model and return n/c- and lg/c-ratios
#'
#' @param subtype Switch between different parameter files
#' @return List of magpie objects with results on global level
#'
#' @author Kristine Karstens
#' @examples
#' \dontrun{
#'   readSource("CENTURY", subtype="tree")
#' }
#'
#' @importFrom reshape2 colsplit
#' @importFrom utils untar write.csv

readCENTURY <- function(subtype="tree"){

  files  <- c(tree="tree.100")
  file   <- toolSubtypeSelect(subtype,files)
  folder <- grep(".zip", list.files(), value=TRUE)
  x      <- readLines(paste0("PARAMETER_FILES/", file))

  out   <- NULL
  full  <- NULL
  type  <- NULL

  for(i in 1:(length(x)/112)){

    tmp        <- x[(1:112)+112*(i-1)]
    header     <- trimws(colsplit(tmp[1], " .", names=c("short","comment")))
    plantparts <- c("leaves", "fine_roots", "fine_branches", "large_wood", "coarse_roots")
    parameters <- c("nmin","nmax","lg")
    cratio     <- new.magpie("GLO","y2000", names = as.vector(outer(parameters,plantparts, paste, sep=".")))
    cratio[,,"nmin"] <- 1/unlist(colsplit(trimws(tmp)[c( 9,12,15,18,21)], " .", names=c("value","name"))[1])
    cratio[,,"nmax"] <- 1/unlist(colsplit(trimws(tmp)[c(24,27,30,33,36)], " .", names=c("value","name"))[1])
    cratio[,,"lg"]   <- unlist(colsplit(trimws(tmp)[c(89:93)], " .", names=c("value","name"))[1])

    tmp_out <- new.magpie("GLO",NULL, names = paste(header[1],c("nc_ratio","lgc_ratio"),sep="."))
    #tmp_out[,,"nc_ratio"]  <- mean(cratio[,,c("nmin","nmax")])
    #tmp_out[,,"lgc_ratio"] <- mean(cratio[,,"lg"])

    out  <- mbind(out, tmp_out)
    full <- mbind(full, add_dimension(cratio, add="type", nm=header[1]))
    type <- rbind(type, paste(header,collapse = " - "))
    write.csv(type, "TreeTypes.csv")
  }

  return(out)
}
