



parseSourceDefinition <- function(src_id, src_def, base_dir){
  
  # YAML fields that correspond directly to slots of the Source class
  # (i.e. everything except format, but note we also manipulate 'dir' below)
  direct_args <- slotNames("Source")[!slotNames("Source") %in% c("format")]
  
  # build a list of args for each YAML entry
  src_args <- list(Class = "Source", id = src_id)
  for(this_entry in names(src_def)) {
    # if it is a direct argument it will be passed straight to the benchmark object (via do.call)
    if(this_entry %in% direct_args)  src_args[[this_entry]] <- src_def[[this_entry]]
    # if it the 'datasets' argument it will be processed into DGVMTools:Source objects
    else if(this_entry == "format") {
      src_args[[this_entry]] <- get(src_def[[this_entry]])
    }
  }
  
  # use base_dir if necessary
  if(!missing(base_dir)) src_args[["dir"]] <- file.path(base_dir, src_args[["dir"]])

  # make Source and return it
  this_src <- do.call(what = "new", args = src_args)
  return(this_src)
  
}


#' @export

parseConfigFile <- function(yml_file) {
  
  input <- yaml::yaml.load_file(yml_file)
  
  # Global settings are easy
  config <- input$Config
  settings <- input$Settings
  
  
  #### MODELS ####
  # For models we loop over Models and create Source objects
  models <- list()
  for(this_model_id in names(input$Models)){
    models[[this_model_id]] <- parseSourceDefinition(this_model_id, input$Models[[this_model_id]])
  }
  settings$simulation_sources <- models
  
  
  #### BENCHMARKS ####
  
  # YAML fields that correspond directly to slots of the Benchmark class
  direct_args <- slotNames("Benchmark")[!slotNames("Benchmark") %in% c("datasets", "custom")]
  
  benchmarks <- list()
  # for each benchmark
  for(this_benchmark_id in names(input$Benchmarks)) {
    
    
    # TODOD - consider factorising out
    this_bmk <- input$Benchmarks[[this_benchmark_id]]
    datasets_list <- list()
    bmk_custom <- list()
    benckmark_args <- list(Class = "Benchmark", id = this_benchmark_id)
    
    # for each YAML entry
    for(this_entry in names(this_bmk)) {
      
      # if it is a direct argument it will be passed straight to the benchmark object (via do.call)
      if(this_entry %in% direct_args)  benckmark_args[[this_entry]] <- this_bmk[[this_entry]]
      # if it the 'datasets' argument it will be processed into DGVMTools:Source objects
      else if(this_entry == "datasets") {
        for(this_dataset in names(this_bmk[[this_entry]])){
          datasets_list[[this_dataset]] <- parseSourceDefinition(this_dataset, this_bmk[[this_entry]][[this_dataset]], config$data_directory)
        }
      }
      # if it is anything else it will be put into the fully flexible 'custom' part 
      else {
        bmk_custom[[this_entry]] <- this_bmk[[this_entry]]
      }
      
    }
    benckmark_args[["datasets"]] <- datasets_list
    benckmark_args[["custom"]] <- bmk_custom
    benchmarks[[this_benchmark_id]] <- do.call(what = "new", args = benckmark_args)
    
  } # for each benchmark
  
  
  #### RETURN ####
  return(
    list(config = config,
         settings = settings,
         models = models,
         benchmarks = benchmarks)
  )
  
}