#' Create benchmark
#'
#' @param benchmark_name The name of the current benchmark, 
#' should follow the benchmark object name from ins file.
#' @param input YAML ins file
#' @param simulation Specifies the simulation to work with. 
#' Value should be integer specifying the correct simulation name from the list in ins.
#' @param Data.year.mean The dataset defined previously in benchmark panel, 
#' can be empty and should be left as is.
#' @param spatial.extent The spatial extent set through yaml ins, 
#' default is NULL which use the full extent of your fields.
#'
#' @return the current benchmark class on which further analysis will be based.
#' @export
#'
#' @examples this_benchmark <- createBenchmark(benchmark_name, input, simulation = 2, Data.year.mean, spatial.extent)
#' @author Karl Piltz (karl.piltz@@nateko.lu.se)
createBenchmark <- function(benchmark_name, input, simulation, Data.year.mean, spatial.extent) {
  
  this_benchmark <- new("Benchmark",
      id = input[[benchmark_name]][["Id"]],
      name = input[[benchmark_name]][["Name"]],
      description = input[[benchmark_name]][["Description"]],
      file_name = input[[benchmark_name]][["File_name"]],
      simulation = input[["Directory"]][["Simulation_name"]][[simulation]],
      spatial_extent_id = input[["Directory"]][["spatial_extent_id"]],
      spatial.extent = spatial.extent,
      guess_var = input[[benchmark_name]][["Layer"]],
      guess_layers = input[[benchmark_name]][["Layer"]], 
      unit = input[[benchmark_name]][["Unit"]],
      agg.unit = input[[benchmark_name]][["Unit"]],
      datasets = list(Data.year.mean),
      dataset_source = input[[benchmark_name]][["Dataset_source"]],
      first.year = as.numeric(input[[benchmark_name]][["First_year"]]),
      last.year = as.numeric(input[[benchmark_name]][["Last_year"]]),
      limits = list(input[[benchmark_name]][["Limits"]]),
      breaks = list(input[[benchmark_name]][["Breaks"]]),
      ax_limits = list(input[[benchmark_name]][["Axis_lim"]]),
      metrics = c("r2","NME","RMSE")
  )
  return(this_benchmark)
}

