
parseBenchmarkDefinition <- function(bmk_id, bmk_def, base_dir){
  
  # YAML fields that correspond directly to slots of the Benchmark class
  direct_args <- slotNames("Benchmark")[!slotNames("Benchmark") %in% c("datasets", "custom")]
  
  # lists etc to be build for then making the benchmark
  datasets_list <- list()
  bmk_custom <- list()
  benckmark_args <- list(Class = "Benchmark", id = bmk_id)
  
  # for each YAML entry add it it the the lists
  for(this_entry in names(bmk_def)) {
    
    # if it is a direct argument it will be passed straight to the benchmark object (via do.call)
    if(this_entry %in% direct_args)  benckmark_args[[this_entry]] <- bmk_def[[this_entry]]
    # if it the 'datasets' argument it will be processed into DGVMTools:Source objects
    else if(this_entry == "datasets") {
      for(this_dataset in names(bmk_def[[this_entry]])){
        datasets_list[[this_dataset]] <- parseSourceDefinition(this_dataset, bmk_def[[this_entry]][[this_dataset]], base_dir)
      }
    }
    # if it is anything else it will be put into the fully flexible 'custom' part 
    else {
      bmk_custom[[this_entry]] <- bmk_def[[this_entry]]
    }
    
  }
  
  # use the lists to build the final argument list and call "new" to make the Benchmark object and return it
  benckmark_args[["datasets"]] <- datasets_list
  benckmark_args[["custom"]] <- bmk_custom
  final_benchmark <- do.call(what = "new", args = benckmark_args)
  return(final_benchmark)
  
}


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
  this_src <- do.call(what = "defineSource", args = src_args)
  return(this_src)
  
}


#' @export

parseConfigFile <- function(yml_file, cli_args) {
  
  
  # read the YAML file
  input <- yaml::yaml.load_file(yml_file)
  
  #### SETTINGS ####
  # Global settings are easy - there are just read directly into one big list (of lists)
  settings <- input$Settings
  
  # override directoies from YAML is value is given on command line
  if(!is.null(cli_args$data_directory)) settings$data_directory <- cli_args$data_directory
  if(!is.null(cli_args$land_cover_file)) settings$land_cover_file <- cli_args$land_cover_file
  
  
  #### SIMULATIONS ####
  simulations <- list()
  # If simulations specified on the command line
  if(!is.null(cli_args$sim_directory) && !is.null(cli_args$sim_name) && !is.null(cli_args$reference_sim_directory) && !is.null(cli_args$reference_sim_name)){
    simulations[[cli_args$sim_name]] <- defineSource(
      name =cli_args$sim_name,
      dir = cli_args$sim_directory,
      format = GUESS)
    simulations[[cli_args$reference_sim_name]] <- defineSource(
      name = cli_args$reference_sim_name,
      dir = cli_args$reference_sim_directory,
      format = GUESS)
    # also override whatever the YAML says for "reference_sim_name" with the command line argument
    settings$reference_sim_name <- cli_args$reference_sim_name
  }
  # Else we loop over Simulations and create Source objects
  else {
    for(this_model_id in names(input$Simulations)){
      simulations[[this_model_id]] <- parseSourceDefinition(this_model_id, input$Simulations[[this_model_id]])
    }
  }
  # also update the "sim_name" and "reference_sim_name"
  if(!is.null(cli_args$sim_name)) settings$sim_name <- cli_args$sim_name
  if(!is.null(cli_args$reference_sim_name)) settings$reference_sim_name <- cli_args$reference_sim_name
  
  # TODO - might need some flexibility concerning the "spatial.extent" setting here to read, for example, gridlists or bounding boxes.
  
  #### BENCHMARKS ####
  # For models we loop over Benchmarks and create Benchmark objects
  benchmarks <- list()
  # for each benchmark
  for(this_benchmark_id in names(input$Benchmarks)) {
    benchmarks[[this_benchmark_id]] <- parseBenchmarkDefinition(this_benchmark_id, input$Benchmarks[[this_benchmark_id]], settings$data_directory)
  } # for each benchmark
  
  
  #### RETURN ####
  return(
    list(settings = settings,
         simulations = simulations,
         benchmarks = benchmarks)
  )
  
}