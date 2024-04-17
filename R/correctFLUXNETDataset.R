#' Corrects Coordinates and station names for FLUXNET
#'
#' @param Dataset FLUXNET Dataset
#'
#' @return Corrected Dataset
#' @export
#'
#' @examples correctFLUXNETDataset(Datasets = Data)
correctFLUXNETDataset <- function(Datasets){
  
FLUXNET_SITES <- read.csv(file.path(system.file("extdata/FLUXNET/FLUXNET_SITES.csv", package = "DGVMBenchmarks")), header = T,sep = ";")

setDT(FLUXNET_SITES)

for(i in seq_along(Datasets)){
# Perform join and add "Site" column to all_Fields_list[[1]]@data
Datasets[[i]]@data[
  FLUXNET_SITES,
  c("Lat", "Lon") := .(i.Lat, i.Lon),
  on = "Name"
]
if ("Name.y" %in% names(Datasets[[i]]@data)){
  rename(Name = Name.y)
}
if ("Code.y" %in% names(Datasets[[i]]@data)){
  rename(Code = Code.y)
}
}
return(Datasets)
}