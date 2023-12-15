#' Process the current dataset to be used in benchmark. 
#' Checks if directory and file exists and then processes the dataset source to get field based on ins file.
#'
#' @param all_datasets List of available datasets. (Can be empty)
#' @param input YAML input file
#' @param benchmark_name name of current benchmark needed to direct input file
#' @param simulation integer telling which simulation to use from the input list
#' @param dataset can be changed if multiple datasets are used. Default is 1.
#' @param layer Default = NULL (Selects all layers in dataset) overwrite with your input input[[benchmark_name]][["Layer"]]
#' @return Data.year.mean (fully processed dataset field)
#' @export
#'
#' @examples Data.year.mean <- processDataSource(all_datasets, input, benchmark_name, simulation = 1)
#' @author Karl Piltz (karl.piltz@@nateko.lu.se)
processDataSource <- function(all_datasets, input, benchmark_name, simulation, spatial.extent, layer = NULL, dataset = 1) {
  # If no dataset present, Data.year.mean is set to empty list.
  if (length(all_datasets) == 0) {
    Data.year.mean <- all_datasets
    return(Data.year.mean)
  } else {
    # If the dataset is present, check if the directory and file path are valid.
    this_dataset_run_dir <- file.path(all_datasets[[dataset]]@dir, input[["Directory"]][["Simulation_name"]][[simulation]])
    
    if (dir.exists(this_dataset_run_dir) &&
        (file.exists(file.path(this_dataset_run_dir, paste0(input[[benchmark_name]][["File_name"]], ".out"))) ||
         file.exists(file.path(this_dataset_run_dir, paste0(input[[benchmark_name]][["File_name"]], ".out.gz"))))) {
      
      this_data_Source <- all_datasets[[dataset]]
      this_data_Source@dir <- file.path(this_dataset_run_dir)
      
      
      if (!is.null(input[[benchmark_name]][["First_year_Data"]]) && !is.null(input[[benchmark_name]][["Last_year_Data"]])){
        Data.year.mean <- DGVMTools::getField(
        source = this_data_Source,
        quant = input[[benchmark_name]][["File_name"]],
        layers = layer,
        first.year = as.numeric(input[[benchmark_name]][["First_year_Data"]]),
        last.year = as.numeric(input[[benchmark_name]][["Last_year_Data"]]),
        spatial.extent.id = input[["Directory"]][["spatial_extent_id"]],
        spatial.extent = spatial.extent,
        year.aggregate.method = "mean"
      )}
        else{
          Data.year.mean <- DGVMTools::getField(
            source = this_data_Source,
            quant = input[[benchmark_name]][["File_name"]],
            layers = layer,
            spatial.extent.id = input[["Directory"]][["spatial_extent_id"]],
            spatial.extent = spatial.extent,
            year.aggregate.method = "mean")
        
      }
      
      Data.year.mean@source@name <- input[[benchmark_name]][["Dataset_name"]]
      return(Data.year.mean)
    }
  }
}



