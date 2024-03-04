#' Title
#'
#' @param all_Fields_list dataset in all_Fields_list
#' @param site.type Choose between "ecosystem" stations or "atmospheric" stations 
#'
#' @return Corrected Lat Lon for ICOS dataset
#' @export
#'
#' @examples
#' @author Karl Piltz (karl.piltz@@nateko.lu.se)
correctICOSDataset <- function(all_Fields_list, site.type = NULL){


stations <- read.csv(file.path(system.file("extdata/ICOS/ICOS_stations_info.csv", package = "DGVMBenchmarks")), header = T,sep = ";")
stations$Lon <- as.numeric(sapply(strsplit(stations$Position, " "), `[`, 1))
stations$Lat <- as.numeric(sapply(strsplit(stations$Position, " "), `[`, 2))
stations <- stations[, -which(names(stations) == "Position")]
stations$Station_ID <- sapply(strsplit(stations$Id, "/"), function(x) tail(x, 1))

stations_dt <- as.data.table(stations)

if (site.type == "Ecosystem"){
matching_ids <- intersect(gsub("^ES_", "", stations$Station_ID), unique(all_Fields_list[[1]]@data$Code))
}
if (site.type == "Atmospheric"){
matching_ids <- intersect(gsub("^AS_", "", stations$Station_ID), unique(all_Fields_list[[1]]@data$Code))
}

# Merge Lon and Lat from matching stations to all_Fields_list[[1]]@data
for (id in matching_ids) {
  idx <- which(all_Fields_list[[1]]@data$Code == id)
  all_Fields_list[[1]]@data[idx, c("Lon", "Lat")] <- stations[stations$Station_ID == paste0("ES_", id), c("Lon", "Lat")]
    }

}