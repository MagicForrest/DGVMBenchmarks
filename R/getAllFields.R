
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
  all_sim_full <- list()
  
  first.year = NULL
  last.year = NULL
  year.aggregate.method = NULL
  spatial.aggregate.method = NULL
  
  if (!is.null(benchmark@first.year) && length(benchmark@first.year) != 0){first.year <- benchmark@first.year}
  if (!is.null(benchmark@last.year) && length(benchmark@last.year) != 0){last.year <- benchmark@last.year}
  if (!is.null(benchmark@year.aggregate.method) && length(benchmark@year.aggregate.method) != 0){year.aggregate.method <- benchmark@year.aggregate.method}
  if (!is.null(benchmark@spatial.aggregate.method) && length(benchmark@spatial.aggregate.method) != 0){spatial.aggregate.method <- benchmark@spatial.aggregate.method}
  
  
  
  if (length(benchmark@datasets) != 0) {
    for (i in seq_along(benchmark@datasets))
      all_Fields_list[[benchmark@datasets[[i]]@source@name]] <- benchmark@datasets[[i]]
  }
  
  if (benchmark@simulation_format == "SITE") {
    for (this_sim_Source in all_simulation_Sources_list[["SITE"]]) {
      
      # check if file is present (if not don't include this run)
      this_benchmark_run_dir <- file.path(this_sim_Source@dir, benchmark@simulation)
      if (file.exists(file.path(this_benchmark_run_dir, paste0(benchmark@file_name, ".csv")))){        
        # make local sources pointing to the simulation directory
        this_Source <- this_sim_Source
        this_Source@dir <- this_benchmark_run_dir
        
        # read the data
        this_simulation <- DGVMTools::getField(source = this_Source,
                                               quant = benchmark@file_name,
                                               first.year = first.year,
                                               last.year = last.year,
                                               layers = benchmark@guess_layers,
                                               units = benchmark@unit,
                                               verbose = verbose_read,
                                               quick.read = quick_read,
                                               quick.read.file = paste(benchmark@id, version_label, sep = "_")
                                               
        )
        setKeyDGVM(this_simulation@data)
        
        all_sim_full[[this_sim_Source@name]] <- this_simulation
        all_Fields_list[[this_sim_Source@name]] <- this_simulation
        if (this_sim_Source@name %in% this_benchmark@Layer_to_convert){
          all_sim_full[[this_sim_Source@name]]@data[[benchmark@guess_layers]] <- all_sim_full[[this_sim_Source@name]]@data[[benchmark@guess_layers]] * as.numeric(this_benchmark@conversion_factor)
          all_Fields_list[[this_sim_Source@name]]@data[[benchmark@guess_layers]] <- all_Fields_list[[this_sim_Source@name]]@data[[benchmark@guess_layers]] * as.numeric(this_benchmark@conversion_factor)
        } 
        
      }
    }
    
    
  }else if (length(benchmark@datasets) != 0 && 
            (benchmark@datasets[[1]]@source@format@id == "ICOS" || 
             benchmark@datasets[[1]]@source@format@id == "FLUXNET") && 
            benchmark@file_name == "GPP")  {
    if(benchmark@simulation_format == "GUESS"){all_simulation_Sources <- all_simulation_Sources_list[["GUESS"]]}
    if(benchmark@simulation_format == "NetCDF"){all_simulation_Sources <- all_simulation_Sources_list[["NetCDF"]]}
    for (this_sim_Source in all_simulation_Sources) {
      
      # check if file is present (if not don't include this run)
      this_benchmark_run_dir <- file.path(this_sim_Source@dir, benchmark@simulation)
      if (file.exists(file.path(this_benchmark_run_dir, paste0("dgpp", ".out"))) ||  file.exists(file.path(this_benchmark_run_dir, paste0("dgpp", ".out.gz"))) ||  file.exists(file.path(this_benchmark_run_dir, paste0("dgpp", ".nc")))) {
        
        # make local sources pointing to the simulation directory
        this_Source <- this_sim_Source
        this_Source@dir <- this_benchmark_run_dir
        
        # read the data
        this_simulation <- DGVMTools::getField(source = this_Source,
                                               quant = "dgpp",
                                               first.year = first.year,
                                               last.year = last.year,
                                               layers = NULL,
                                               units = benchmark@unit,
                                               verbose = verbose_read,
                                               quick.read = quick_read,
                                               quick.read.file = paste(benchmark@id, version_label, sep = "_")
        )
        
        if ("Forest_sum" %in% names(this_simulation@data)){this_simulation@data <- this_simulation@data %>%
          rename(GPP = Forest_sum)}
        if ("Total" %in% names(this_simulation@data)){this_simulation@data <- this_simulation@data %>%
          rename(GPP = Total)}
        
        
        all_sim_full[[this_sim_Source@name]] <- this_simulation
        all_Fields_list[[this_sim_Source@name]] <- this_simulation
        if (this_sim_Source@name %in% this_benchmark@Layer_to_convert){
          all_sim_full[[this_sim_Source@name]]@data[[benchmark@guess_layers]] <- all_sim_full[[this_sim_Source@name]]@data[[benchmark@guess_layers]] * as.numeric(this_benchmark@conversion_factor)
          all_Fields_list[[this_sim_Source@name]]@data[[benchmark@guess_layers]] <- all_Fields_list[[this_sim_Source@name]]@data[[benchmark@guess_layers]] * as.numeric(this_benchmark@conversion_factor)
        } 
      }
    }
  } else if (length(benchmark@datasets) != 0 &&
             (benchmark@datasets[[1]]@source@format@id == "ICOS" || 
              benchmark@datasets[[1]]@source@format@id == "FLUXNET") &&
             benchmark@file_name == "NEE") {
    if(benchmark@simulation_format == "GUESS"){all_simulation_Sources <- all_simulation_Sources_list[["GUESS"]]}
    if(benchmark@simulation_format == "NetCDF"){all_simulation_Sources <- all_simulation_Sources_list[["NetCDF"]]}
    
    for (this_sim_Source in all_simulation_Sources) {
      
      # check if file is present (if not don't include this run)
      this_benchmark_run_dir <- file.path(this_sim_Source@dir, benchmark@simulation)
      if (file.exists(file.path(this_benchmark_run_dir, paste0("dnee", ".out"))) ||  file.exists(file.path(this_benchmark_run_dir, paste0("dnee", ".out.gz"))) ||  file.exists(file.path(this_benchmark_run_dir, paste0("dnee", ".nc")))) {
        
        # make local sources pointing to the simulation directory
        this_Source <- this_sim_Source
        this_Source@dir <- this_benchmark_run_dir
        
        # read the data
        this_simulation <- DGVMTools::getField(source = this_Source,
                                               quant = "dnee",
                                               first.year = first.year,
                                               last.year = last.year,
                                               layers = NULL,
                                               units = benchmark@unit,
                                               verbose = verbose_read,
                                               quick.read = quick_read,
                                               quick.read.file = paste(benchmark@id, version_label, sep = "_")
        )
        
        if ("Forest_sum" %in% names(this_simulation@data)){this_simulation@data <- this_simulation@data %>%
          rename(NEE = Forest_sum)}
        if ("Total" %in% names(this_simulation@data)){this_simulation@data <- this_simulation@data %>%
          rename(NEE = Total)}
        
        all_sim_full[[this_sim_Source@name]] <- this_simulation
        all_Fields_list[[this_sim_Source@name]] <- this_simulation
        if (this_sim_Source@name %in% this_benchmark@Layer_to_convert){
          all_sim_full[[this_sim_Source@name]]@data[[benchmark@guess_layers]] <- all_sim_full[[this_sim_Source@name]]@data[[benchmark@guess_layers]] * as.numeric(this_benchmark@conversion_factor)
          all_Fields_list[[this_sim_Source@name]]@data[[benchmark@guess_layers]] <- all_Fields_list[[this_sim_Source@name]]@data[[benchmark@guess_layers]] * as.numeric(this_benchmark@conversion_factor)
        } 
      }
    }
  }else if (length(benchmark@datasets) != 0 &&
            (benchmark@datasets[[1]]@source@format@id == "ICOS" ||
             benchmark@datasets[[1]]@source@format@id == "FLUXNET") &&
            benchmark@file_name == "Reco") {
    if(benchmark@simulation_format == "GUESS"){all_simulation_Sources <- all_simulation_Sources_list[["GUESS"]]}
    if(benchmark@simulation_format == "NetCDF"){all_simulation_Sources <- all_simulation_Sources_list[["NetCDF"]]}
    
    for (this_sim_Source in all_simulation_Sources) {
      
      # check if file is present (if not don't include this run)
      this_benchmark_run_dir <- file.path(this_sim_Source@dir, benchmark@simulation)
      if (file.exists(file.path(this_benchmark_run_dir, paste0("dreco", ".out"))) ||  file.exists(file.path(this_benchmark_run_dir, paste0("dreco", ".out.gz"))) ||  file.exists(file.path(this_benchmark_run_dir, paste0("dreco", ".nc")))) {
        
        # make local sources pointing to the simulation directory
        this_Source <- this_sim_Source
        this_Source@dir <- this_benchmark_run_dir
        
        # read the data
        this_simulation <- DGVMTools::getField(source = this_Source,
                                               quant = "dreco",
                                               first.year = first.year,
                                               last.year = last.year,
                                               layers = NULL,
                                               units = benchmark@unit,
                                               verbose = verbose_read,
                                               quick.read = quick_read,
                                               quick.read.file = paste(benchmark@id, version_label, sep = "_")
        )
        
        if ("Forest_sum" %in% names(this_simulation@data)){this_simulation@data <- this_simulation@data %>%
          rename(Reco = Forest_sum)}
        if ("Total" %in% names(this_simulation@data)){this_simulation@data <- this_simulation@data %>%
          rename(Reco = Total)}
        
        all_sim_full[[this_sim_Source@name]] <- this_simulation
        all_Fields_list[[this_sim_Source@name]] <- this_simulation
        if (this_sim_Source@name %in% this_benchmark@Layer_to_convert){
          all_sim_full[[this_sim_Source@name]]@data[[benchmark@guess_layers]] <- all_sim_full[[this_sim_Source@name]]@data[[benchmark@guess_layers]] * as.numeric(this_benchmark@conversion_factor)
          all_Fields_list[[this_sim_Source@name]]@data[[benchmark@guess_layers]] <- all_Fields_list[[this_sim_Source@name]]@data[[benchmark@guess_layers]] * as.numeric(this_benchmark@conversion_factor)
        } 
      }
    }
  }else {
    if(benchmark@simulation_format == "GUESS"){all_simulation_Sources <- all_simulation_Sources_list[["GUESS"]]}
    if(benchmark@simulation_format == "NetCDF"){all_simulation_Sources <- all_simulation_Sources_list[["NetCDF"]]}
    
    for (this_sim_Source in all_simulation_Sources) {
      
      
      # check if file is present (if not don't include this run)
      this_benchmark_run_dir <- file.path(this_sim_Source@dir, benchmark@simulation)
      if (file.exists(file.path(this_benchmark_run_dir, paste0(benchmark@file_name, ".out"))) ||
          file.exists(file.path(this_benchmark_run_dir, paste0(benchmark@file_name, ".out.gz"))) ||
          file.exists(file.path(this_benchmark_run_dir, paste0(benchmark@file_name, ".nc")))){        
        # make local sources pointing to the simulation directory
        this_Source <- this_sim_Source
        this_Source@dir <- this_benchmark_run_dir
        
        # read the data
        this_simulation <- DGVMTools::getField(source = this_Source,
                                               quant = benchmark@file_name,
                                               first.year = first.year,
                                               last.year = last.year,
                                               spatial.extent.id = benchmark@spatial_extent_id,
                                               spatial.extent = benchmark@spatial.extent,
                                               layers = benchmark@guess_layers,
                                               units = benchmark@unit,
                                               verbose = verbose_read,
                                               quick.read = quick_read,
                                               quick.read.file = paste(benchmark@id, version_label, sep = "_"),
                                               year.aggregate.method = year.aggregate.method,
                                               spatial.aggregate.method = spatial.aggregate.method
        )
        
        all_sim_full[[this_sim_Source@name]] <- this_simulation
        all_Fields_list[[this_sim_Source@name]] <- this_simulation
        if (this_sim_Source@name %in% this_benchmark@Layer_to_convert){
          all_sim_full[[this_sim_Source@name]]@data[[benchmark@guess_layers]] <- all_sim_full[[this_sim_Source@name]]@data[[benchmark@guess_layers]] * as.numeric(this_benchmark@conversion_factor)
          all_Fields_list[[this_sim_Source@name]]@data[[benchmark@guess_layers]] <- all_Fields_list[[this_sim_Source@name]]@data[[benchmark@guess_layers]] * as.numeric(this_benchmark@conversion_factor)
        } 
      }
    }
    
  }
  
  return(list(all_sim_full, all_Fields_list))
}
