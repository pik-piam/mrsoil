#' @title readSanderman
#' @description Read validation data from Sanderman et al., 2017
#'
#' @return List of magpie objects with results on cellular level, weight, unit and description.
#' @author Kristine Karstens
#'
#' @examples
#' \dontrun{
#' readSource("Sanderman")
#' }
#'
#' @import madrat
#' @importFrom readxl read_excel
#' @importFrom magpiesets Cell2Country
#' @importFrom stats complete.cases

readSanderman <- function() {

  data        <- as.data.frame(subset(read_excel("remnant_native_SOC_database_for_release.xlsx", sheet = "sites"),
                                      select = c("Latitude", "Longitude", "Native/Ag/Restored", "30 cm SOC")))
  data$ID     <- c(1:nrow(data))

  raster2half    <- function(x) {
    return(trunc(x * 2) / 2 + sign(x) * 0.25)
  }
  data$lat  <- raster2half(data$Latitude)
  data$lon  <- raster2half(data$Longitude)

  data           <- data[complete.cases(data), ]
  names(data)    <- c("Latitude", "Longitude", "Native", "SOC", "ID", "lat", "lon")

  magpie_coord   <- Cell2Country()
  data           <- subset(merge(magpie_coord, data, by = c("lon", "lat")),
                           select = c("Longitude", "Latitude", "Native", "celliso", "SOC", "ID"))
  data$Native    <- 1*(data$Native == "Native")
  data$celliso   <- paste(data$celliso, data$ID, sep = ".")

  out            <- new.magpie(data$celliso, NULL, names = names(data)[-c(4)],
                               fill = as.matrix(data[, c(1:3, 5:6)]),
                               sets = c("iso.N", "years", "data"))
  return(out)
}
