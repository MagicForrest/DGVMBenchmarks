
#' GetAllFields, processing the actual fields from your sources based on the benchmark instructions
#'
#' @param benchmark Holding current benchmark instructions for getfield call
#' @param all_simulation_Sources_list list of all simulation sources
#'
#' @return list of lists containing all simulation fields and all fields including data
#' @export
#'
#' @examples getAllFields(benchmark = this_benchmark, all_simulation_Sources_list = all_simulation_Sources_list)
#' @author Karl Piltz (karl.piltz@@nateko.lu.se)
getAllFields <- function(benchmark = this_benchmark, all_simulation_Sources_list){

all_Fields_list <- list()
if(length(benchmark@datasets[[1]]) != 0){
all_Fields_list[[benchmark@datasets[[1]]@source@name]] <- benchmark@datasets[[1]]}

all_sim_full <- list()
for(this_sim_Source in all_simulation_Sources_list) {

  # check if file is present (if not don't include this run)
  this_benchmark_run_dir <- file.path(this_sim_Source@dir, benchmark@simulation)
  if(file.exists(file.path(this_benchmark_run_dir, paste0(benchmark@file_name, ".out"))) ||  file.exists(file.path(this_benchmark_run_dir, paste0(benchmark@file_name,".out.gz")))) {

    # make local sources pointing to the simulation directory
    this_Source <- this_sim_Source
    this_Source@dir <- this_benchmark_run_dir

    # read the data
    this_simulation <- DGVMTools::getField(source = this_Source,
                                           quant = benchmark@file_name,
                                           first.year = benchmark@first.year,
                                           last.year = benchmark@last.year,
                                           spatial.extent.id = benchmark@spatial_extent_id,
                                           spatial.extent = benchmark@spatial.extent,
                                           layers = benchmark@guess_layers,
                                           units = benchmark@unit,
                                           verbose = verbose_read,
                                           quick.read = quick_read,
                                           quick.read.file = paste(benchmark@id, version_label, sep = "_"),
                                           year.aggregate.method = "mean"
    )

    all_sim_full[[this_sim_Source@name]] <- this_simulation
    all_Fields_list[[this_sim_Source@name]] <- this_simulation
  }
}
return(list(all_sim_full,all_Fields_list))
}



