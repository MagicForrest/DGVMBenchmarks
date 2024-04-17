#' @export

parseConfigFile <- function(yml_file) {
  
  input <- yaml::yaml.load_file(yml_file)
  
  config <- input$Settings
  
  benchmarks <-list()
  
  return(
    list(config = config,
         benchmarks = benchmarks)
  )
  
}