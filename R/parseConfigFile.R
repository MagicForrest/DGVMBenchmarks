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
    
    # TODO Validity checks here
    this_model_name <- input$Models[[this_model_id]]$name
    this_model_dir <- input$Models[[this_model_id]]$dir
    this_model_format_str <- input$Models[[this_model_id]]$format
    this_model_format <- get(this_model_format_str)
    
    models[[this_model_id]] <- defineSource(id = this_model_id,
                               name = this_model_name,
                               dir = this_model_dir,
                               format = this_model_format)
    
  }
  settings$simulation_sources <- models
  
  
  #### BENCHMARKS ####
  benchmarks <- list()
  
  return(
    list(config = config,
         settings = settings,
         models = models,
         benchmarks = benchmarks)
  )
  
}