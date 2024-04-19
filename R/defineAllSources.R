
#' Define all sources
#'
#' @param input yaml instruction file holding Benchmark Formats
#' 
#'
#' @return List of lists for the dataset source and the simulations sources
#' @export
#'
#' @examples defineAllSources(input = input_data)
#' @author Karl Piltz (karl.piltz@@nateko.lu.se)
defineAllSources <- function(input){
  all_formats <- c()
  for (i in 3:length(input)) {
    # Extract Data$Format from the current object and add it to the all_formats vector
    all_formats <- c(all_formats, input[[i]]$Data$Format)
    # Extract Simulations$Format from the current object and add it to the all_formats vector
    all_formats <- c(all_formats, input[[i]]$Simulations$Format)
  }
  
  # Get all Formats to be processed
  Formats <- unique(all_formats)
  all_datasets <- list()
  all_simulation_Sources_list <- list()
  ## Set Model and reference sources ##
  all_GUESS_simulation_Sources_list <- list()
  if ("GUESS" %in% Formats){
    GUESS_sources <- DGVMBenchmarks::define_GUESS_Sources(input = input)
    all_GUESS_datasets <- GUESS_sources[[1]]
    all_datasets[["GUESS"]] <- all_GUESS_datasets
    all_GUESS_simulation_Sources_list <- GUESS_sources[[2]]
    all_simulation_Sources_list[["GUESS"]] <- all_GUESS_simulation_Sources_list
  }
  
  all_NetCDF_simulation_Sources_list <- list()
  if ("NetCDF" %in% Formats){
    NetCDF_sources <- DGVMBenchmarks::define_NetCDF_Sources(input = input)
    all_NetCDF_datasets <- NetCDF_sources[[1]]
    all_datasets[["NetCDF"]] <- all_NetCDF_datasets
    all_NetCDF_simulation_Sources_list <- NetCDF_sources[[2]]
    all_simulation_Sources_list[["NetCDF"]] <- all_NetCDF_simulation_Sources_list}
  
  all_SITE_simulation_Sources_list <- list()
  if ("SITE" %in% Formats){
    SITE_sources <- DGVMBenchmarks::define_SITE_Sources(input = input)
    all_SITE_datasets <- SITE_sources[[1]]
    all_datasets[["SITE"]] <- all_SITE_datasets
    all_SITE_simulation_Sources_list <- SITE_sources[[2]]
    all_simulation_Sources_list[["SITE"]] <- all_SITE_simulation_Sources_list}
  
  if ("ICOS" %in% Formats){
    all_ICOS_datasets <- DGVMBenchmarks::define_ICOS_DatasetSource(input = input)
    all_datasets[["ICOS"]] <- all_ICOS_datasets
  }
  
  if ("FLUXNET" %in% Formats){
    all_FLUXNET_datasets <- DGVMBenchmarks::define_FLUXNET_DatasetSource(input = input)
    all_datasets[["FLUXNET"]] <- all_FLUXNET_datasets
  }
  
  all_aDGVM_simulation_Sources_list <- list()
  if ("aDGVM" %in% Formats){
    aDGVM_sources <- DGVMBenchmarks::define_aDGVM_Sources(input = input)
    all_aDGVM_datasets <- aDGVM_sources[[1]]
    all_datasets[["aDGVM"]] <- all_aDGVM_datasets
    all_aDGVM_simulation_Sources_list <- aDGVM_sources[[2]]
    all_simulation_Sources_list[["aDGVM"]] <- all_aDGVM_simulation_Sources_list
  }
  
  all_aDGVM2_simulation_Sources_list <- list()
  if ("aDGVM2" %in% Formats){
    aDGVM2_sources <- DGVMBenchmarks::define_aDGVM2_Sources(input = input)
    all_aDGVM2_datasets <- aDGVM2_sources[[1]]
    all_datasets[["aDGVM2"]] <- all_aDGVM2_datasets
    all_aDGVM2_simulation_Sources_list <- aDGVM2_sources[[2]]
    all_simulation_Sources_list[["aDGVM2"]] <- all_aDGVM2_simulation_Sources_list
  }
  return(sources <- list(all_datasets,all_simulation_Sources_list))
}
