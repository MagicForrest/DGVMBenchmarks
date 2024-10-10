
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
  adgvm.file.type = "Yearly"
  adgvm.fire = 1
  adgvm.climate = 0
  adgvm2.scheme = 1
  adgvm2.daily = TRUE
  
  if (!is.null(benchmark@first.year) && length(benchmark@first.year) != 0){first.year <- benchmark@first.year}
  if (!is.null(benchmark@last.year) && length(benchmark@last.year) != 0){last.year <- benchmark@last.year}
  if (!is.null(benchmark@year.aggregate.method) && length(benchmark@year.aggregate.method) != 0){year.aggregate.method <- benchmark@year.aggregate.method}
  if (!is.null(benchmark@spatial.aggregate.method) && length(benchmark@spatial.aggregate.method) != 0){spatial.aggregate.method <- benchmark@spatial.aggregate.method}
  if (!is.null(benchmark@adgvm.file.type) && length(benchmark@adgvm.file.type) != 0){adgvm.file.type <- benchmark@adgvm.file.type}
  if (!is.null(benchmark@adgvm.fire) && length(benchmark@adgvm.fire) != 0){adgvm.fire <- benchmark@adgvm.fire}
  if (!is.null(benchmark@adgvm.climate) && length(benchmark@adgvm.climate) != 0){adgvm.climate <- benchmark@adgvm.climate}
  if (!is.null(benchmark@adgvm2.scheme) && length(benchmark@adgvm2.scheme) != 0){adgvm2.scheme <- benchmark@adgvm2.scheme}
  if (!is.null(benchmark@adgvm2.daily) && length(benchmark@adgvm2.daily) != 0){adgvm2.daily <- benchmark@adgvm2.daily}
  
  
  
  
  
  
  if (length(benchmark@datasets) != 0) {
    for (i in seq_along(benchmark@datasets))
      all_Fields_list[[benchmark@datasets[[i]]@source@name]] <- benchmark@datasets[[i]]
  }
  
  if (length(benchmark@simulation_format) == 1 && length(all_simulation_Sources_list[[1]]) != 1) {
    duplicated_format <- benchmark@simulation_format[1]
    benchmark@simulation_format <- c(benchmark@simulation_format, duplicated_format)
    duplicated_simulation <- benchmark@simulation[1]
    benchmark@simulation <- c(benchmark@simulation, duplicated_simulation)
    duplicated_filename <- benchmark@file_name[1]
    benchmark@file_name <- c(benchmark@file_name, duplicated_filename)}
    
    for (format_index in seq_along(benchmark@simulation_format)) {
      current_format <- benchmark@simulation_format[[format_index]]
  
  
  
  
  if (current_format == "SITE") {
      
      # check if file is present (if not don't include this run)
      this_benchmark_run_dir <- file.path(all_simulation_Sources_list[["SITE"]][[format_index]]@dir, benchmark@simulation[[format_index]])
      if (file.exists(file.path(this_benchmark_run_dir, paste0(benchmark@file_name[[format_index]], ".csv")))){        
        # make local sources pointing to the simulation directory
        this_Source <- all_simulation_Sources_list[["SITE"]][[format_index]]
        this_Source@dir <- this_benchmark_run_dir
        
        # read the data
        this_simulation <- DGVMTools::getField(source = this_Source,
                                               quant = benchmark@file_name[[format_index]],
                                               first.year = first.year,
                                               last.year = last.year,
                                               layers = benchmark@guess_layers,
                                               units = benchmark@unit,
                                               verbose = verbose_read,
                                               quick.read = quick_read,
                                               quick.read.file = paste(benchmark@id, version_label, sep = "_")
                                               
        )
        setKeyDGVM(this_simulation@data)
        
        all_sim_full[[this_Source@name]] <- this_simulation
        all_Fields_list[[this_Source@name]] <- this_simulation
        if (this_Source@name %in% this_benchmark@Layer_to_convert){
          all_sim_full[[this_Source@name]]@data[[benchmark@guess_layers]] <- all_sim_full[[this_Source@name]]@data[[benchmark@guess_layers]] * as.numeric(benchmark@conversion_factor)
          all_Fields_list[[this_Source@name]]@data[[benchmark@guess_layers]] <- all_Fields_list[[this_Source@name]]@data[[benchmark@guess_layers]] * as.numeric(benchmark@conversion_factor)
        } 
        
      }
  }else if (length(benchmark@datasets) != 0 && 
            (benchmark@datasets[[1]]@source@format@id == "ICOS" || 
             benchmark@datasets[[1]]@source@format@id == "FLUXNET"))  {
    if(current_format == "GUESS"){this_sim_source <- all_simulation_Sources_list[["GUESS"]][[format_index]]}
    if(current_format == "NetCDF"){this_sim_source <- all_simulation_Sources_list[["NetCDF"]][[format_index]]}
      
      # check if file is present (if not don't include this run)
      this_benchmark_run_dir <- file.path(this_sim_source@dir, benchmark@simulation[[format_index]])
      if (file.exists(file.path(this_benchmark_run_dir, paste0(benchmark@file_name[[format_index]], ".out"))) ||  file.exists(file.path(this_benchmark_run_dir, paste0(benchmark@file_name[[format_index]], ".out.gz"))) ||  file.exists(file.path(this_benchmark_run_dir, paste0(benchmark@file_name[[format_index]], ".nc")))) {
        
        # make local sources pointing to the simulation directory
        this_Source <- this_sim_source
        this_Source@dir <- this_benchmark_run_dir
        
        # read the data
        this_simulation <- DGVMTools::getField(source = this_Source,
                                               quant = benchmark@file_name[[format_index]],
                                               first.year = first.year,
                                               last.year = last.year,
                                               layers = NULL,
                                               units = benchmark@unit,
                                               verbose = verbose_read,
                                               quick.read = quick_read,
                                               quick.read.file = paste(benchmark@id, version_label, sep = "_")
        )
        if (this_simulation@quant@id == "dgpp"){
        if ("Forest_sum" %in% names(this_simulation@data)){this_simulation@data <- this_simulation@data %>%
          rename(GPP = Forest_sum)}
        if ("Total" %in% names(this_simulation@data)){this_simulation@data <- this_simulation@data %>%
          rename(GPP = Total)}}
        if (this_simulation@quant@id == "daet"){
          if ("Forest_sum" %in% names(this_simulation@data)){this_simulation@data <- this_simulation@data %>%
            rename(ET = Forest_sum)}
          if ("Total" %in% names(this_simulation@data)){this_simulation@data <- this_simulation@data %>%
            rename(ET = Total)}}
        
        
        all_sim_full[[this_sim_source@name]] <- this_simulation
        all_Fields_list[[this_sim_source@name]] <- this_simulation
        if (this_sim_source@name %in% this_benchmark@Layer_to_convert){
          all_sim_full[[this_sim_source@name]]@data[[benchmark@guess_layers]] <- all_sim_full[[this_sim_source@name]]@data[[benchmark@guess_layers]] * as.numeric(benchmark@conversion_factor)
          all_Fields_list[[this_sim_source@name]]@data[[benchmark@guess_layers]] <- all_Fields_list[[this_sim_source@name]]@data[[benchmark@guess_layers]] * as.numeric(benchmark@conversion_factor)
        } 
      }
  }else if (current_format == "aDGVM"){
    this_sim_Source <- all_simulation_Sources_list[["aDGVM"]][[format_index]]
    
    
    # check if file is present (if not don't include this run)
    this_benchmark_run_dir <- file.path(this_sim_Source@dir, benchmark@simulation[[format_index]])
    if (file.exists(file.path(this_benchmark_run_dir, paste0(benchmark@file_name[[format_index]], ".nc")))){        
      # make local sources pointing to the simulation directory
      this_Source <- this_sim_Source
      this_Source@dir <- this_benchmark_run_dir
      
      # read the data
      this_simulation <- DGVMTools::getField(source = this_Source,
                                             quant = benchmark@file_name[[format_index]],
                                             first.year = first.year,
                                             last.year = last.year,
                                             spatial.extent.id = benchmark@spatial_extent_id,
                                             spatial.extent = benchmark@spatial.extent,
                                             layers = benchmark@guess_layers,
                                             units = benchmark@unit,
                                             verbose = verbose_read,
                                             adgvm.file.type = adgvm.file.type,
                                             adgvm.fire = adgvm.fire,
                                             adgvm.climate = adgvm.climate,
                                             quick.read = quick_read,
                                             quick.read.file = paste(benchmark@id, version_label, sep = "_"),
                                             year.aggregate.method = year.aggregate.method,
                                             spatial.aggregate.method = spatial.aggregate.method
      )
      
      all_sim_full[[this_sim_Source@name]] <- this_simulation
      all_Fields_list[[this_sim_Source@name]] <- this_simulation
      if (this_sim_Source@name %in% this_benchmark@Layer_to_convert){
        all_sim_full[[this_sim_Source@name]]@data[[benchmark@guess_layers]] <- all_sim_full[[this_sim_Source@name]]@data[[benchmark@guess_layers]] * as.numeric(benchmark@conversion_factor)
        all_Fields_list[[this_sim_Source@name]]@data[[benchmark@guess_layers]] <- all_Fields_list[[this_sim_Source@name]]@data[[benchmark@guess_layers]] * as.numeric(benchmark@conversion_factor)
      } 
    }
  }else if (current_format == "aDGVM2"){
    this_sim_Source <- all_simulation_Sources_list[["aDGVM2"]][[format_index]]
    
    
    # check if file is present (if not don't include this run)
    this_benchmark_run_dir <- file.path(this_sim_Source@dir, benchmark@simulation[[format_index]])
    if (file.exists(file.path(this_benchmark_run_dir, paste0(benchmark@file_name[[format_index]], ".nc")))){        
      # make local sources pointing to the simulation directory
      this_Source <- this_sim_Source
      this_Source@dir <- this_benchmark_run_dir
      
      # read the data
      this_simulation <- DGVMTools::getField(source = this_Source,
                                             quant = benchmark@file_name[[format_index]],
                                             first.year = first.year,
                                             last.year = last.year,
                                             spatial.extent.id = benchmark@spatial_extent_id,
                                             spatial.extent = benchmark@spatial.extent,
                                             layers = benchmark@guess_layers,
                                             units = benchmark@unit,
                                             verbose = verbose_read,
                                             adgvm2.scheme = adgvm2.scheme,
                                             adgvm2.daily = adgvm2.daily,
                                             quick.read = quick_read,
                                             quick.read.file = paste(benchmark@id, version_label, sep = "_"),
                                             year.aggregate.method = year.aggregate.method,
                                             spatial.aggregate.method = spatial.aggregate.method
      )
      
      all_sim_full[[this_sim_Source@name]] <- this_simulation
      all_Fields_list[[this_sim_Source@name]] <- this_simulation
      if (this_sim_Source@name %in% this_benchmark@Layer_to_convert){
        all_sim_full[[this_sim_Source@name]]@data[[benchmark@guess_layers]] <- all_sim_full[[this_sim_Source@name]]@data[[benchmark@guess_layers]] * as.numeric(benchmark@conversion_factor)
        all_Fields_list[[this_sim_Source@name]]@data[[benchmark@guess_layers]] <- all_Fields_list[[this_sim_Source@name]]@data[[benchmark@guess_layers]] * as.numeric(benchmark@conversion_factor)
      } 
    }
  }else {
    if(current_format == "GUESS"){this_sim_Source <- all_simulation_Sources_list[["GUESS"]][[format_index]]}
    if(current_format == "NetCDF"){this_sim_Source <- all_simulation_Sources_list[["NetCDF"]][[format_index]]}
      
      # check if file is present (if not don't include this run)
      this_benchmark_run_dir <- file.path(this_sim_Source@dir, benchmark@simulation[[format_index]])
      if (file.exists(file.path(this_benchmark_run_dir, paste0(benchmark@file_name[[format_index]], ".out"))) ||
          file.exists(file.path(this_benchmark_run_dir, paste0(benchmark@file_name[[format_index]], ".out.gz"))) ||
          file.exists(file.path(this_benchmark_run_dir, paste0(benchmark@file_name[[format_index]], ".nc")))){        
        # make local sources pointing to the simulation directory
        this_Source <- this_sim_Source
        this_Source@dir <- this_benchmark_run_dir
        
        if (current_format == "GUESS"){
        # read the data
        this_simulation <- DGVMTools::getField(source = this_Source,
                                               quant = benchmark@file_name[[format_index]],
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
        )}
        if (current_format == "NetCDF"){
          # read the data
          this_simulation <- DGVMTools::getField(source = this_Source,
                                                 quant = benchmark@file_name[[format_index]],
                                                 first.year = first.year,
                                                 last.year = last.year,
                                                 spatial.extent.id = benchmark@spatial_extent_id,
                                                 spatial.extent = benchmark@spatial.extent,
                                                 layers = benchmark@guess_layers,
                                                 verbose = verbose_read,
                                                 quick.read = quick_read,
                                                 quick.read.file = paste(benchmark@id, version_label, sep = "_"),
                                                 year.aggregate.method = year.aggregate.method,
                                                 spatial.aggregate.method = spatial.aggregate.method
          )}
        
        all_sim_full[[this_sim_Source@name]] <- this_simulation
        all_Fields_list[[this_sim_Source@name]] <- this_simulation
        if (this_sim_Source@name %in% this_benchmark@Layer_to_convert){
          all_sim_full[[this_sim_Source@name]]@data[[benchmark@guess_layers]] <- all_sim_full[[this_sim_Source@name]]@data[[benchmark@guess_layers]] * as.numeric(benchmark@conversion_factor)
          all_Fields_list[[this_sim_Source@name]]@data[[benchmark@guess_layers]] <- all_Fields_list[[this_sim_Source@name]]@data[[benchmark@guess_layers]] * as.numeric(benchmark@conversion_factor)
        } 
      }
    }
    }
  
  
  return(list(all_sim_full, all_Fields_list))
}
