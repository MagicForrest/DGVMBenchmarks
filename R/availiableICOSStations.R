
#' List all availiable ICOS stations and get Lat/Lon corrections.
#'
#' @param Stations Default "ALL" renders datatable with all availiable stations.
#' @param station.selection Make a selection by station name to get correct Lat/Lon. Takes singe chr vector or list of vectors.
#'
#' @return datatable with station information.
#' @export
#'
#' @examples
#' @author Karl Piltz (karl.piltz@@nateko.lu.se)
availiableICOSStations <- function(Stations = NULL, station.selection= NULL){

stations <- read.csv(file.path(system.file("extdata/ICOS/ICOS_stations_info.csv", package = "DGVMBenchmarks")), header = T,sep = ";")
stations$Lon <- as.numeric(sapply(strsplit(stations$Position, " "), `[`, 1))
stations$Lat <- as.numeric(sapply(strsplit(stations$Position, " "), `[`, 2))
stations <- stations[, -which(names(stations) == "Position")]
stations$Station_ID <- sapply(strsplit(stations$Id, "/"), function(x) tail(x, 1))

if (!is.null(station.selection)){
available_stations <- unique(stations[, c("Name", "Station_ID")])
return(DT::datatable(available_stations, 
          caption = "Available Stations", 
          options = list(pageLength = 172 , dom = 't')))
}else if (!is.null(station.selection)) {
  station.selection <- as.character(station.selection)
  station.selection <- station.selection[station.selection %in% stations$Name]
  if (length(station.selection) == 0) {
    stop("Specified station names not found.")
  }
  specified_stations_data <- stations[stations$Name %in% station.selection, c("Station_ID","Name", "Lon", "Lat")]
  return(DT::datatable(specified_stations_data, 
                       caption = "Specified Stations", 
                       options = list(pageLength = nrow(specified_stations_data), dom = 't')))
} else {
  stop("Invalid argument. Use 'All' to get all stations or provide a vector of specified station names.")
}
}






