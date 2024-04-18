#' Process the current dataset to be used in benchmark. 
#' Checks if directory and file exists and then processes the dataset source to get field based on ins file.
#'
#' @param all_datasets List of available datasets. (Can be empty)
#' @param input YAML input file
#' @param benchmark_name name of current benchmark needed to direct input file
#' @param layer Default = NULL (Selects all layers in dataset) overwrite with your input input[[benchmark_name]][["Layer"]]
#' @return Data.year.mean (fully processed dataset field)
#' @export
#'
#' @examples Data.year.mean <- processDataSource(all_datasets, input, benchmark_name, simulation = 1)
#' @author Karl Piltz (karl.piltz@@nateko.lu.se)
processDataSource <- function(all_datasets, input, benchmark_name, spatial.extent, layer = NULL) {
  
  Datasets <- list()
  
  UT.threshold = "VUT"
  partition.method = "REF"
  day.night.method = "NT"
  NEE.day.night = NULL
  year.aggregate.method = NULL
  spatial.aggregate.method = NULL
  first.year = NULL
  last.year = NULL
  rm.leap = TRUE
  data.cleaning = TRUE
  qc.threshold = 0.5
  
  if (!is.null(input[[benchmark_name]][["Data"]][["First_year"]]) && length(input[[benchmark_name]][["Data"]][["First_year"]]) != 0){first.year <- as.numeric(input[[benchmark_name]][["Data"]][["First_year"]])}
  if (!is.null(input[[benchmark_name]][["Data"]][["Last_year"]]) && length(input[[benchmark_name]][["Data"]][["Last_year"]]) != 0){last.year <- as.numeric(input[[benchmark_name]][["Data"]][["Last_year"]])}
  if (!is.null(input[[benchmark_name]][["Data"]][["year.aggregate.method"]]) && length(input[[benchmark_name]][["Data"]][["year.aggregate.method"]]) != 0){year.aggregate.method <- input[[benchmark_name]][["Data"]][["year.aggregate.method"]]}
  if (!is.null(input[[benchmark_name]][["Data"]][["spatial.aggregate.method"]]) && length(input[[benchmark_name]][["Data"]][["spatial.aggregate.method"]]) != 0){spatial.aggregate.method <- input[[benchmark_name]][["Data"]][["spatial.aggregate.method"]]}
  if (!is.null(input[[benchmark_name]][["Data"]][["ICOS_FLUXNET_Specific"]][["NEE.day.night"]]) && length(input[[benchmark_name]][["Data"]][["ICOS_FLUXNET_Specific"]][["NEE.day.night"]]) != 0){NEE.day.night <- input[[benchmark_name]][["Data"]][["ICOS_FLUXNET_Specific"]][["NEE.day.night"]]}
  if (!is.null(input[[benchmark_name]][["Data"]][["ICOS_FLUXNET_Specific"]][["day.night.method"]]) && length(input[[benchmark_name]][["Data"]][["ICOS_FLUXNET_Specific"]][["day.night.method"]]) != 0){day.night.method <- input[[benchmark_name]][["Data"]][["ICOS_FLUXNET_Specific"]][["day.night.method"]]}
  if (!is.null(input[[benchmark_name]][["Data"]][["ICOS_FLUXNET_Specific"]][["partition.method"]]) && length(input[[benchmark_name]][["Data"]][["ICOS_FLUXNET_Specific"]][["partition.method"]]) != 0){partitioning.method <- input[[benchmark_name]][["Data"]][["ICOS_FLUXNET_Specific"]][["partition.method"]]}
  if (!is.null(input[[benchmark_name]][["Data"]][["ICOS_FLUXNET_Specific"]][["UT.threshold"]]) && length(input[[benchmark_name]][["Data"]][["ICOS_FLUXNET_Specific"]][["UT.threshold"]]) != 0){UT.threshold <- input[[benchmark_name]][["Data"]][["ICOS_FLUXNET_Specific"]][["UT.threshold"]]}
  if (!is.null(input[[benchmark_name]][["Data"]][["ICOS_FLUXNET_Specific"]][["rm.leap"]]) && length(input[[benchmark_name]][["Data"]][["ICOS_FLUXNET_Specific"]][["rm.leap"]]) != 0){rm.leap <- as.logical(input[[benchmark_name]][["Data"]][["ICOS_FLUXNET_Specific"]][["rm.leap"]])}
  if (!is.null(input[[benchmark_name]][["Data"]][["ICOS_FLUXNET_Specific"]][["data.cleaning"]]) && length(input[[benchmark_name]][["Data"]][["ICOS_FLUXNET_Specific"]][["data.cleaning"]]) != 0){data.cleaning <- as.logical(input[[benchmark_name]][["Data"]][["ICOS_FLUXNET_Specific"]][["data.cleaning"]])}
  if (!is.null(input[[benchmark_name]][["Data"]][["ICOS_FLUXNET_Specific"]][["qc.threshold"]]) && length(input[[benchmark_name]][["Data"]][["ICOS_FLUXNET_Specific"]][["qc.threshold"]]) != 0){qc.threshold <- as.numeric(input[[benchmark_name]][["Data"]][["ICOS_FLUXNET_Specific"]][["qc.threshold"]])}
  
  
  # If no dataset present, Data.year.mean is set to empty list.
  if (length(input[[benchmark_name]][["Data"]][["Datasets"]]) == 0) {
    return(Datasets)
  } else{
    for (i in seq_along(input[[benchmark_name]][["Data"]][["Datasets"]])){
      if (input[[benchmark_name]][["Data"]][["Format"]][[i]] == "SITE"){
        this_dataset_run_dir <- file.path(all_datasets[["SITE"]][[1]]@dir, input[[benchmark_name]][["Data"]][["Datasets"]][[i]])
        
        if(file.exists(file.path(this_dataset_run_dir, paste0(input[[benchmark_name]][["Data"]][["File_name"]][[i]], ".csv")))){
          this_data_Source <- all_datasets[["SITE"]][[1]]
          this_data_Source@dir <- this_dataset_run_dir
          
          Data.year.mean <- DGVMTools::getField(source = this_data_Source, 
                                                quant = input[[benchmark_name]][["Data"]][["File_name"]][[i]],
                                                layers = input[[benchmark_name]][["Layer"]],
                                                units = input[[benchmark_name]][["Unit"]],
                                                verbose = verbose_read,
                                                quick.read = quick_read,
                                                quick.read.file = paste(input[[benchmark_name]][["Id"]], version_label, sep = "_"))
          Data.year.mean@source@name <- input[[benchmark_name]][["Data"]][["Dataset_name"]][[i]]
          
          
          ## PROFOUND data exception
          if ("species_id" %in% colnames(Data.year.mean@data)) {species <- unique(Data.year.mean@data$species_id)}
          if (length(species) > 1) {
            Data.year.mean@data <- Data.year.mean@data[, .(Forest_sum = sum(get(input[[benchmark_name]][["Layer"]]))), by = .(Lat,Lon,Year,Site)]
            setnames(Data.year.mean@data, "Forest_sum", input[[benchmark_name]][["Layer"]])
          }
          
          if (Data.year.mean@source@name %in% input[[benchmark_name]][["Data"]][["Layer_to_convert"]]){
            Data.year.mean@data[[input[[benchmark_name]][["Layer"]]]] <- Data.year.mean@data[[input[[benchmark_name]][["Layer"]]]] * as.numeric(input[[benchmark_name]][["Data"]][["conversion_factor"]])
          }
          setKeyDGVM(Data.year.mean@data)
          Datasets[[Data.year.mean@source@name]] <- Data.year.mean
          
        } 
      }else if (input[[benchmark_name]][["Data"]][["Format"]][[i]] == "ICOS") {
        # If the dataset is present, check if the directory and file path are valid.
        this_dataset_run_dir <- file.path(all_datasets[["ICOS"]][[1]]@dir, input[[benchmark_name]][["Data"]][["Datasets"]][[i]])
        
        # Check precence of files with daily fluxes and siteinfo in ICOS directory
        daily.files <- list.files(this_dataset_run_dir) %>%
          stringr::str_subset("DD")
        siteinfo.files <- list.files(this_dataset_run_dir) %>% stringr::str_subset("SITEINFO")
        
        if (dir.exists(this_dataset_run_dir) &&
            (length(daily.files) != 0 ) && 
            (length(siteinfo.files) != 0) &&
            (length(daily.files) == length(siteinfo.files))){
          
          this_data_Source <- all_datasets[["ICOS"]][[1]]
          this_data_Source@dir <- file.path(this_dataset_run_dir)  
          
          Data.year.mean <- DGVMTools::getField(
            source = this_data_Source,
            quant = input[[benchmark_name]][["Data"]][["File_name"]][[i]],
            layers = NULL,
            UT.threshold = UT.threshold,
            partition.method = partition.method,
            day.night.method = day.night.method,
            NEE.day.night = NEE.day.night,
            rm.leap = rm.leap,
            data.cleaning = data.cleaning,
            qc.threshold = qc.threshold,
            first.year = first.year,
            last.year = last.year,
            quick.read = quick_read,
            quick.read.file = paste(benchmark_name, version_label, sep = "_"))
          
          
          
          Data.year.mean@source@name <- input[[benchmark_name]][["Data"]][["Dataset_name"]][[i]]
          
          if (Data.year.mean@source@name %in% input[[benchmark_name]][["Data"]][["Layer_to_convert"]]){
            Data.year.mean@data[[input[[benchmark_name]][["Layer"]]]] <- Data.year.mean@data[[input[[benchmark_name]][["Layer"]]]] * as.numeric(input[[benchmark_name]][["Data"]][["conversion_factor"]])
          } 
          
          Datasets[[Data.year.mean@source@name]] <- Data.year.mean
          
        }
      }else if (input[[benchmark_name]][["Data"]][["Format"]][[i]] == "FLUXNET") {
        # If the dataset is present, check if the directory and file path are valid.
        this_dataset_run_dir <- file.path(all_datasets[["FLUXNET"]][[1]]@dir, input[[benchmark_name]][["Data"]][["Datasets"]][[i]])
        
        # Check precence of files with daily fluxes and siteinfo in ICOS directory
        daily.files <- list.files(this_dataset_run_dir) %>%
          stringr::str_subset("DD") %>%
          stringr::str_subset("ERAI", negate = T) %>% stringr::str_subset("AA", negate = T)
        
        
        if (dir.exists(this_dataset_run_dir) &&
            (length(daily.files) != 0)){
          
          this_data_Source <- all_datasets[["FLUXNET"]][[1]]
          this_data_Source@dir <- file.path(this_dataset_run_dir)  
          
          Data.year.mean <- DGVMTools::getField(
            source = this_data_Source,
            quant = input[[benchmark_name]][["Data"]][["File_name"]][[i]],
            layers = NULL,
            UT.threshold = UT.threshold,
            partition.method = partition.method,
            day.night.method = day.night.method,
            NEE.day.night = NEE.day.night,
            rm.leap = rm.leap,
            data.cleaning = data.cleaning,
            qc.threshold = qc.threshold,
            first.year = first.year,
            last.year = last.year,
            quick.read = quick_read,
            quick.read.file = paste(benchmark_name, version_label, sep = "_"))
          
          
          
          
          
          Data.year.mean@source@name <- input[[benchmark_name]][["Data"]][["Dataset_name"]][[i]]
          
          if (Data.year.mean@source@name %in% input[[benchmark_name]][["Data"]][["Layer_to_convert"]]){
            Data.year.mean@data[[input[[benchmark_name]][["Layer"]]]] <- Data.year.mean@data[[input[[benchmark_name]][["Layer"]]]] * as.numeric(input[[benchmark_name]][["Data"]][["conversion_factor"]])
          } 
          Datasets[[Data.year.mean@source@name]] <- Data.year.mean
          
        }
      }else if (input[[benchmark_name]][["Data"]][["Format"]][[i]] == "GUESS"){
        # If the dataset is present, check if the directory and file path are valid.
        this_dataset_run_dir <- file.path(all_datasets[["GUESS"]][[1]]@dir, input[[benchmark_name]][["Data"]][["Datasets"]][[i]])
        
        if (dir.exists(this_dataset_run_dir) &&
            (file.exists(file.path(this_dataset_run_dir, paste0(input[[benchmark_name]][["Data"]][["File_name"]][[i]], ".out"))) ||
             file.exists(file.path(this_dataset_run_dir, paste0(input[[benchmark_name]][["Data"]][["File_name"]][[i]], ".out.gz"))))) {
          
          this_data_Source <- all_datasets[["GUESS"]][[1]]
          this_data_Source@dir <- file.path(this_dataset_run_dir)
          
          
          Data.year.mean <- DGVMTools::getField(
            source = this_data_Source,
            quant = input[[benchmark_name]][["Data"]][["File_name"]][[i]],
            layers = input[[benchmark_name]][["Layer"]],
            first.year = first.year,
            last.year = last.year,
            spatial.extent.id = input[["Directory"]][["spatial_extent_id"]],
            spatial.extent = spatial.extent,
            year.aggregate.method = year.aggregate.method,
            spatial.aggregate.method = spatial.aggregate.method
          )
          
          
          
          Data.year.mean@source@name <- input[[benchmark_name]][["Data"]][["Dataset_name"]][[i]]
          
          if (Data.year.mean@source@name %in% input[[benchmark_name]][["Data"]][["Layer_to_convert"]]){
            Data.year.mean@data[[input[[benchmark_name]][["Layer"]]]] <- Data.year.mean@data[[input[[benchmark_name]][["Layer"]]]] * as.numeric(input[[benchmark_name]][["Data"]][["conversion_factor"]])
          } 
          Datasets[[Data.year.mean@source@name]] <- Data.year.mean
          
        }
      }else if (input[[benchmark_name]][["Data"]][["Format"]][[i]] == "NetCDF"){
        # If the dataset is present, check if the directory and file path are valid.
        this_dataset_run_dir <- file.path(all_datasets[["NetCDF"]][[1]]@dir, input[[benchmark_name]][["Data"]][["Datasets"]][[i]])
        
        if (dir.exists(this_dataset_run_dir) &&
            (file.exists(file.path(this_dataset_run_dir, paste0(input[[benchmark_name]][["Data"]][["File_name"]][[i]], ".nc"))) ||
             file.exists(file.path(this_dataset_run_dir, paste0(input[[benchmark_name]][["Data"]][["File_name"]][[i]], ".nc.gz"))))) {
          
          this_data_Source <- all_datasets[["NetCDF"]][[1]]
          this_data_Source@dir <- file.path(this_dataset_run_dir)
          
          
          Data.year.mean <- DGVMTools::getField(
            source = this_data_Source,
            quant = input[[benchmark_name]][["Data"]][["File_name"]][[i]],
            layers = input[[benchmark_name]][["Layer"]],
            first.year = first.year,
            last.year = last.year,
            spatial.extent.id = input[["Directory"]][["spatial_extent_id"]],
            spatial.extent = spatial.extent,
            year.aggregate.method = year.aggregate.method,
            spatial.aggregate.method = spatial.aggregate.method
          )
          
          
          
          Data.year.mean@source@name <- input[[benchmark_name]][["Data"]][["Dataset_name"]][[i]]
          
          if (Data.year.mean@source@name %in% input[[benchmark_name]][["Data"]][["Layer_to_convert"]]){
            Data.year.mean@data[[input[[benchmark_name]][["Layer"]]]] <- Data.year.mean@data[[input[[benchmark_name]][["Layer"]]]] * as.numeric(input[[benchmark_name]][["Data"]][["conversion_factor"]])
          } 
          Datasets[[Data.year.mean@source@name]] <- Data.year.mean
          
        }
      }
    }
    return(Datasets)
  }
}
