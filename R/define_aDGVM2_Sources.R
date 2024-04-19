#' Define aDGVM2 sources. Create a aDGVM2 source for your dataset and simulations. 
#'
#' @param input 
#'
#' @return List of lists for the dataset source and the simulations sources
#' @export
#'
#' @examples
#' @author Karl Piltz (karl.piltz@@nateko.lu.se)
define_aDGVM2_Sources <- function(input){
  ## Set Model and reference sources ##
  all_aDGVM2_datasets <- list()
  if (is.na(input[["Directory"]][["Data"]]) || input[["Directory"]][["Data"]] == ""){input[["Directory"]][["Data"]] <- NULL}
  if (!is.null(input[["Directory"]][["Data"]])){
    Reference_data <- DGVMTools::defineSource(id = "Obs", dir = input[["Directory"]][["Data"]], format = aDGVM2, name = "Obs")
    all_aDGVM2_datasets <- list(Reference_data)}
  
  # Define source for latest model run
  New_run <- DGVMTools::defineSource(id = input[["Directory"]][["Sim1_id"]], dir = input[["Directory"]][["Sim1"]], format = aDGVM2, name = input[["Directory"]][["Sim1_id"]])
  # If previous model run is to be included, defined here. All sources added to list for benchmarking.
  if (is.na(input[["Directory"]][["Sim2"]]) || input[["Directory"]][["Sim2"]] == ""){input[["Directory"]][["Sim2"]] <- NULL}
  if (!is.null(input[["Directory"]][["Sim2"]])){Old_run <- DGVMTools::defineSource(id = input[["Directory"]][["Sim2_id"]], dir = input[["Directory"]][["Sim2"]], format = aDGVM2, name = input[["Directory"]][["Sim2_id"]])
  all_aDGVM2_simulation_Sources_list <- list(New_run, Old_run)
  }else{all_aDGVM2_simulation_Sources_list <- list(New_run)}
  return(aDGVM2_sources <- list(all_aDGVM2_datasets,all_aDGVM2_simulation_Sources_list))
}