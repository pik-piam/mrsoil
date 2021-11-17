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

  data        <- as.data.frame(subset(read_excel("remnant native SOC database for release.xlsx", sheet = "sites"),
                                      select = c("Latitude", "Longitude", "Native/Ag/Restored", "30 cm SOC")))

  raster2half    <- function(x) {
    return(trunc(x * 2) / 2 + sign(x) * 0.25)
  }
  data$Latitude  <- raster2half(data$Latitude)
  data$Longitude <- raster2half(data$Longitude)

  data           <- data[complete.cases(data), ]
  names(data)    <- c("lat", "lon", "native", "soc")

  magpie_coord   <- Cell2Country()
  data           <- subset(merge(magpie_coord, data, by = c("lon", "lat")),
                           select = c("lon", "lat", "native", "celliso", "soc"))
  data$native    <- 1*(data$native == "Native")

  out            <- new.magpie(data$celliso, NULL, names = names(data)[-4],
                               fill = as.matrix(data[, c(1:3, 5)]),
                               sets = c("celliso", "years", "data"))

  return(out)
}
