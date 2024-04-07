#' Corrects Coordinates and station names for FLUXNET
#'
#' @param Dataset FLUXNET Dataset
#'
#' @return Corrected Dataset
#' @export
#'
#' @examples correctFLUXNETDataset(Dataset = Data)
correctFLUXNETDataset <- function(Dataset){
  
FLUXNET_SITES <- read.csv(file.path(system.file("extdata/FLUXNET/FLUXNET_SITES.csv", package = "DGVMBenchmarks")), header = T,sep = ";")

setDT(FLUXNET_SITES)


# Perform join and add "Site" column to all_Fields_list[[1]]@data
Dataset@data[
  FLUXNET_SITES,
  c("Lat", "Lon") := .(i.Lat, i.Lon),
  on = "Name"
]
if ("Name.y" %in% names(Dataset@data)){
  rename(Name = Name.y)
}
if ("Code.y" %in% names(Dataset@data)){
  rename(Code = Code.y)
}

return(Dataset)
}