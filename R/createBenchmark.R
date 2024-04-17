#' Create benchmark
#'
#' @param benchmark_name The name of the current benchmark, 
#' should follow the benchmark object name from ins file.
#' @param input YAML ins file
#' Value should be integer specifying the correct simulation name from the list in ins.
#' @param Datasets The dataset list defined previously in benchmark panel, 
#' can be empty and should be left as is.
#' @param spatial.extent The spatial extent set through yaml ins, 
#' default is NULL which use the full extent of your fields.
#'
#' @return the current benchmark class on which further analysis will be based.
#' @export
#'
#' @examples this_benchmark <- createBenchmark(benchmark_name, input, simulation = 2, Data.year.mean, spatial.extent)
#' @author Karl Piltz (karl.piltz@@nateko.lu.se)
createBenchmark <- function(benchmark_name, input, Datasets, spatial.extent = NULL){
  
  true_statistics <- character()
  for (stat in names(input[[benchmark_name]][["Statistics"]])) {
    if (input[[benchmark_name]][["Statistics"]][[stat]] == "T" || input[[benchmark_name]][["Statistics"]][[stat]] == "TRUE") {
      true_statistics<- true_statistics <- c(true_statistics, stat)
    }
  }
  
  this_benchmark <- new("benchmark",
                        id = input[[benchmark_name]][["Id"]],
                        name = input[[benchmark_name]][["Name"]],
                        description = input[[benchmark_name]][["Description"]],
                        file_name = input[[benchmark_name]][["File_name"]],
                        simulation = input[[benchmark_name]][["Simulations"]][["Simulation"]],
                        spatial_extent_id = input[["Directory"]][["spatial_extent_id"]],
                        spatial.extent = spatial.extent,
                        guess_var = input[[benchmark_name]][["Layer"]],
                        guess_layers = input[[benchmark_name]][["Layer"]], 
                        unit = input[[benchmark_name]][["Unit"]],
                        spatial.aggregate.method = as.character(input[[benchmark_name]][["Simulations"]][["spatial.aggregate.method"]]),
                        year.aggregate.method = as.character(input[[benchmark_name]][["Simulations"]][["year.aggregate.method"]]),
                        agg.unit = input[[benchmark_name]][["Unit"]],
                        datasets = Datasets,
                        dataset_source = input[[benchmark_name]][["Data"]][["Dataset_source"]],
                        simulation_format = input[[benchmark_name]][["Simulations"]][["Format"]],
                        first.year = as.numeric(input[[benchmark_name]][["Simulations"]][["First_year"]]),
                        last.year = as.numeric(input[[benchmark_name]][["Simulations"]][["Last_year"]]),
                        limits = list(input[[benchmark_name]][["Plotting"]][["Spatial_Difference"]][["Limits"]]),
                        breaks = list(input[[benchmark_name]][["Plotting"]][["Spatial_Difference"]][["Breaks"]]),
                        ax_limits = list(input[[benchmark_name]][["Plotting"]][["Spatial_Scatter"]][["Limits"]]),
                        metrics = true_statistics,
                        conversion_factor = as.numeric(input[[benchmark_name]][["Simulations"]][["Conversion_factor"]]),
                        Layer_to_convert = as.list(input[[benchmark_name]][["Simulations"]][["Layer_to_convert"]])
  )
  
  return(this_benchmark)
}

